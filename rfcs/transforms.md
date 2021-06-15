## Motivation

Hasura integrates with external HTTP APIs via a variety of features: actions, event triggers, scheduled triggers. Since the client request that these external APIs expect may be different from what Hasura generates, we want to be able to transform these requests before invoking the external API.

## Spec

We will introduce a key called `transforms` in the configuration for actions, event triggers and scheduled triggers.

The schema for `transforms` is described here:

``` 
data RequestMethod = GET | POST | PUT | PATCH | DELETE

data Transforms 
  = Transforms
  { requestMethod ::  Maybe RequestMethod
 -- ^ Change the request method to one provided here. Nothing means POST.
  , requestURL    ::  Maybe TemplateText
 -- ^ Change the request URL to one provided here. Nothing means original URL.
  , request_body  ::  Maybe TemplateText                  # Raw templating
 -- ^ Change the request_body to one provided here. Nothing means original payload. Only applicable for POST, PUT, PATCH methods
  , query_params  ::  Maybe (HashMap Text TemplateText)   # Key-value based templating
 -- ^ Add query params provided here to URI. Only applicable for GET and DELETE methods
  , request_headers :: Maybe TransformHeaders
 -- ^ Tranform headers as defined here.
  }

data TransformHeaders
  = TransformHeaders { replaceHeaders :: [(HeaderName, HeaderValue}] }

data TemplateText # TO BE DECIDED (see next section)

```

## Templating Language

This is largely undecided at the moment.

We want to be able to use a templating language which can use `JSONPath` expressions to construct the final value. We should also be able to loop through array items. An example is the following where `$` represents root of the original JSON payload.

```
{
  "author": {
    "name": "$.event.name",
    "age": "$.event.age",
    "articles": [
#foreach($article in $.event.author.articles)
      {
        "id": "$article.id",
        "title": $article.title
      }
#end
    ]
  }
}
```

Simple string templating is also required (for example for requestURI transform):

```
# $url has the original URL
$url/$.event.author_id
```

TODO: Explore Haskell templating packages which resemble this.


## Examples

Transform the request body:

```
transforms:
   request_body: |
     {
       "key1": "$.value1",
       "key2": "$.value2",
       "key3": "$.session['x-hasura-user-id']"
     }
```

Tranform the URL based on data in the request body:

```
transforms:
   request_url: "$url/$.input['country']"
```


All webhook integrations (actions, event triggers, scheduled triggers) are POST method based but we can transform the method to `GET` as shown below. Note that, for `GET` methods we should be able to map the payload to the query params.

```
transforms:
   request_method: GET
   query_params:
     param1: "$.value1"
     param2: "$.value2"
```

We should also be able to transform the request headers. Hasura already has a way to add headers hence what will be useful is a way to replace few of the system added headers. The following example changes the body type to `x-www-form-urlencoded` and allows a custom `User-Agent`.

```
transforms:
   request_body: |
     key1=$.value1&key2=$.value2&key3=$.session['x-hasura-user-id']
   request_headers:
     replace_headers:
       Content-Type: application/x-www-form-urlencoded
       User-Agent: myapp-server
```
