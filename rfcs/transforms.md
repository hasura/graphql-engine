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
 -- The encoding of the content is defined by the 'request_content_type' field defined below.
  , request_content_type :: Maybe AllowedContentTypes
 -- ^ Only the following Content-Types are allowed (default: application/json):
 -- 1) application/json
 -- 2) application/x-www-form-urlencoded
  , query_params  ::  Maybe (HashMap Text TemplateText)   # Key-value based templating
 -- ^ Add query params provided here to URL
 -- Each param is URL encoded
  , request_headers :: Maybe TransformHeaders
 -- ^ Tranform headers as defined here.
 -- Certain headers cannot be transformed (e.g. `Content-Type` and should return a warning)
  }

-- | The order of evaluation is removeHeaders and then addHeaders
data TransformHeaders
  = TransformHeaders
  { addHeaders :: [(HeaderName, TemplateText}]
  , removeHeaders :: [HeaderName]
  }


data TemplateText # TO BE DECIDED (see next section)

```

## Templating Language

This is largely undecided at the moment.

We want to be able to use a templating language which can construct JSON payloads (for POST body) as well as raw strings (for URL transform)

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

Simple string templating is also required (for example for requestURL or headers transform):

```
# $url has the original URL
$url/$.event.author_id
```

TODO: Explore Haskell templating packages which resemble this.


## Examples

1. Transform the request body:

```
transforms:
   request_body:
     {
       "key1": "$.value1",
       "key2": "$.value2",
       "key3": "$.session['x-hasura-user-id']"
     }
```

2. Tranform the URL based on data in the request body:

```
transforms:
   request_url: "$url/$.input['country']"
```


3. Transform the method. All webhook integrations (actions, event triggers, scheduled triggers) are POST method based but we can transform the method to `GET` as shown below. Note that, for `GET` methods we should be able to map the payload to the query params and they should be URL encoded.

```
transforms:
   request_method: GET
   query_params:
     param1: "$.value1"
     param2: "$.value2"
```

4. Transform the request headers. For example, below we add a custom header from the event payload and modify the `User-Agent`. As noted in the spec, we first remove headers and then add headers. 

```
transforms:
   request_headers:
     add_headers: [{"x-cutom-id": "$.event.user_id"}, { "User-Agent": myapp-server}]
     remove_headers: [User-Agent]
```

5. Change the request content type. The following example changes the content type to `x-www-form-urlencoded`. Note that we still supply the request_body as a JSON. Every top-level field/value is converted into a form parameter (and value is url encoded).

```
transforms:
   request_body:
     {
       "key1": "$.value1",
       "key2": "$.value2",
       "key3": "$.session['x-hasura-user-id']"
     }
   request_content_type: x-www-form-urlencoded
```

This transforms the body to `key1=$.value1&key2=$.value2&key3=$.session['x-hasura-user-id']`. The console can show the output for a given tranformation hence making it clear what is finally going in the body.

## Real-world example

1. Integrating notion.so API via Hasura Action

Action definition:

```
type Mutation {
  addNotionItem (
    databaseId: String!
    properties: jsonb!
    children: jsonb
  ): ItemOutput
}

type ItemOutput {
  id : String!
  object : String!
  created_time : timestamptz!
  last_edited_time : timestamptz!
}
```

The transform is given by:

```
   request_body:
     {
       "parent": {
          "database_id": action.input.databaseId,
       }.
       "properties": action.input.properties,
       {{ if action.input.children }} "children": action.input.children
     }

```

Source: https://glitch.com/edit/#!/butternut-grizzly-fluorine?path=src%2Fserver.js%3A1%3A0

## Debugging

The server also provides a `v1/metadata` type called `test_http_transformer` which takes the transformer spec and a payload and returns the transformed request.

```
type: test_http_transformer
args:
  payload:
    webhook_url: {{WEBHOOK_URL}} -- note that payload can reference env vars
    event:
      user_id: 1
      username: bob
      password: cat
  transformer:
    request_method: GET
    request_url: {{webhook_url}}/{{event.user_id}}
    query_params:
      param1: "event.username"
      param2: "event.password"
```

Output:

```
request_method: GET
request_url: http://httbin.org/1?param1=bob&param2=cat
```


## Console 

The console DX is crucial for making the feature successful. We need a way to test a template with sample data. For this, the console needs to generate sample data (payload) for a given webhook. After this, the console takes the provided template and passes it to debugging endpoint described in the previous section.
