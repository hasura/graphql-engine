## Motivation

Hasura integrates with external HTTP APIs via a variety of features: actions, event triggers, scheduled triggers. Since the client request that these external APIs expect may be different from what Hasura generates, we want to be able to transform these requests before invoking the external API.

## Spec

We will introduce a key called `transforms` in the configuration for actions, event triggers and scheduled triggers.

The schema for `transforms` is described here:

```
data RequestMethod = GET | POST | PUT | PATCH | DELETE
data ContentType = JSON | XWWWFORM
newtype TemplatingEngine = KritiLang
newtype TemplateText = TemplateText T.Text

data Transforms
  = Transforms
  { requestMethod ::  Maybe RequestMethod
 -- ^ Change the request method to one provided here. Nothing means POST.

  , requestURL    ::  Maybe TemplateText
 -- ^ Change the request URL to one provided here. Nothing means original URL.

  , request_body  ::  Maybe TemplateText
 -- ^ Change the request_body to one provided here. Nothing means original payload. Only applicable for POST, PUT, PATCH methods
 -- The encoding of the content is defined by the 'request_content_type' field defined below.

  , request_content_type :: Maybe ContentType
 -- ^ Only the following Content-Types are allowed (default: application/json):
 -- 1) application/json
 -- 2) application/x-www-form-urlencoded

  , query_params  ::  Maybe (HashMap Text TemplateText)   # Key-value based templating
 -- ^ Add query params provided here to URL
 -- Each param is URL encoded

  , request_headers :: Maybe TransformHeaders
 -- ^ Tranform headers as defined here.
 -- Certain headers cannot be transformed (e.g. `Content-Type` and should return a warning)

   , templating_engine :: Maybe TemplatingEngine
 -- ^ The template engine to use for transformations. Default: go-basic
  }

-- | The order of evaluation is removeHeaders and then addHeaders
type HeaderName = Text
data TransformHeaders
  = TransformHeaders
  { addHeaders :: [(HeaderName, TemplateText}]
  , removeHeaders :: [HeaderName]
  }
```

## Templating Language

We want to be able to use a templating language which can construct JSON payloads (for POST body) as well as raw strings (for URL transform)

We are going to go with a custom templating language which is a subset of `Go` templating language. We will call this `go-basic`.

`go-basic` will initially consist of string templating for objects and arrays, for loops, if/then/else statements, and basic predicate functions (==, <, >, &&, ||).

### String Templating
Useful for example for requestURL or headers transform.

```
# $url has the original URL
$url/{{.event.author_id}}
```

### For Loops
```
{
    "articles": [
{{ range .event.author.articles }}
      {
        "id": {{$.id}},
        "title": {{$.title}}
      }
{{ end }}
}
```

For loops will be declared with the `range` identifier and a template fragment referrencing the array to be looped over. The current array element is brought in scope be accessed as `$`.

We will also support enumeration of array elements with a special syntax:
```
{{ $index, $article := .event.author.articles }}
```
This binds--with the scope of the loop--the array element as `$article` and it's array index as `$index`.

The loop is terminated with the `end` identifier.

### If/Then/Else

If statements can be constructed as follows:
```
{{if $index}}
, "{{$article}}"
{{else}}
  "{{$article}}"
{{end}}
```
We will also provide the following basic predicate functions:
```
$.x == $.y
$.x >  $.y
$.x <  $.y
$.x && $.y
$.x || $.y
```

### A more complete example
```
{
  "author": {
    "name": "{{$.event.name}}",
    "age": "{{$.event.age}}",
    "articles": [
      {{range $index, $article := $.event.author.articles}}
        {{if $index > 5}}
        , "{{$article}}"
        {{else}}
          "{{$article}}"
        {{end}}
      {{end}}
    ]
  }
}
```

## Implementation Notes
We can define an extended json Value type which includes our primitives:
```
data Accessor = Obj String | Arr Int
  deriving (Show, Eq)

data ValueExt =
  -- Core Aeson Terms
    Object (M.HashMap Text ValueExt)
  | Array (V.Vector ValueExt)
  | String Text
  | Number Scientific
  | Boolean Bool
  | Null
  -- Extended Terms
  | Path [Accessor]
  | Iff ValueExt ValueExt ValueExt
  | Eq ValueExt ValueExt
  | Gt ValueExt ValueExt
  | Lt ValueExt ValueExt
  | AND ValueExt ValueExt
  | OR ValueExt ValueExt
  | Member ValueExt ValueExt
  | Range (Maybe Text) Text [Accessor] ValueExt
  -- ^ {{ range i, x := $.foo.bar }}
  deriving (Show, Eq, Read)
```

Here is an example of `go-basic` and it's parsed AST:
```
exampleJson = [r|
{
  "author": {
    "name": {{$.event.name}},
    "age": {{$.event.age}},
    "articles": [
{{ range _, $article $.event.author.articles }}
      {
        "id": {{$article.id}},
        "title": {{$article.title}}
      }
{{ end }}
    ]
  }
}
|]

exampleAst :: ValueExt
exampleAst =
  Object $ M.fromList
  [ ("author"
    , Object $ M.fromList
      [ ("name", Path [Obj "event", Obj "name"])
      , ("age", Path [Obj "event", Obj "age"])
      , ("articles"
        , Range Nothing "$article" (Path [Obj "$", Obj "event", Obj "author", Obj "articles"])
            [ Object $ M.fromList
                [ ("id", Path [Obj "$article", Obj "id"])
                , ("title", Path [Obj "$article", Obj "title"])
                ]
            ]

        )
      ]
    )
  ]
```

Then we write an evaluation function with the type:
```
eval :: ValueExt -> Value -> Either Err Value
```

JSON is untyped and we will want to be aware of the likely runtime errors:
1. Predicates are applied to non-boolean values.
2. The template reference in a loop declaration references an Array.
3. Out of bound array accessors and undefined object accessors.

We will have to wait until runtime to catch these sorts of errors.

## Alternatives
An alternative templating language is `jsonnet`. The main reason for choosing `go-basic` is the ease of the syntax

## Examples

1. Transform the request body:

```
transforms:
   request_body:
     {
       "key1": "{{$.value1}}",
       "key2": "{{$.value2}}",
       "key3": "{{$.session['x-hasura-user-id']}}"
     }
```

2. Tranform the URL based on data in the request body:

```
transforms:
   request_url: "$url/{{$.input['country']}}"
```


3. Transform the method. All webhook integrations (actions, event triggers, scheduled triggers) are POST method based but we can transform the method to `GET` as shown below. Note that, for `GET` methods we should be able to map the payload to the query params and they should be URL encoded.

```
transforms:
   request_method: GET
   query_params:
     param1: {{$.value1}}
     param2: {{$.value2}}
```

4. Transform the request headers. For example, below we add a custom header from the event payload and modify the `User-Agent`. As noted in the spec, we first remove headers and then add headers.

```
transforms:
   request_headers:
     add_headers: [{"x-cutom-id": "{{$.event.user_id}}"}, { "User-Agent": myapp-server}]
     remove_headers: [User-Agent]
```

5. Change the request content type. The following example changes the content type to `x-www-form-urlencoded`. Note that we still supply the request_body as a JSON. Every top-level field/value is converted into a form parameter (and value is url encoded).

```
transforms:
   request_body:
     {
       "key1": "{{$.value1}}",
       "key2": "{{$.value2}}",
       "key3": "{{$session.x-hasura-user-id}}"
     }
   request_content_type: x-www-form-urlencoded
```

This transforms the body to `key1={{.value1}}&key2={{.value2}}&key3={{$session.x-hasura-user-id}}`. The console can show the output for a given tranformation hence making it clear what is finally going in the body.

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
          "database_id": "{{$.action.input.databaseId}}",
       }.
       "properties": "{{$.action.input.properties}}",
       "children":
         {{ if $.action.input.children }}
           {{$.action.input.children}}
         {{ else }}
           null
         {{ end }}
     }

```

Source: https://glitch.com/edit/#!/butternut-grizzly-fluorine?path=src%2Fserver.js%3A1%3A0

## Debugging

The server also provides a `v1/metadata` type called `test_webhook_transformer` which takes the transformer spec and a payload and returns the transformed request.

```
type: test_http_transformer
args:
  webhook_url: http://httbin.org
  payload:
    event:
      user_id: 1
      username: bob
      password: cat
  transformer:
    request_method: GET
    request_url: $url/{{event.user_id}}
    query_params:
      param1: "{{$.event.username}}"
      param2: "{{$.event.password}}"
```

Output:

```
request_method: GET
request_url: http://httbin.org/1?param1=bob&param2=cat
```

For console editor integration we also provide a second endpoint
`validate_go-basic_template` which takes a template and a json
value. This endpoint will attempt to parse and evaluate the template
with the json data and return the results.

```
{
    "type" : "test_webhook_transform",
    "args" : {
        "webhook_url": "https://localhost:1234",
        "body": { "hello": "world" },
        "request_transform": {
          "body": "{{ $.hello }}",
          "template_engine": "Kriti"
         }
    }
}
```

Happy Output:
```
{
  "payload": "world",
  "headers": [
    [
      "content-type",
      "application/json"
    ]
  ],
  "method": "GET",
  "webhook_url": "https://localhost:1234/"
}
```
Failure Output:
```
{
  "payload": {
    "error_code": "TypeErrorCode",
    "source_position": {
      "end_column": 15,
      "start_line": 1,
      "end_line": 1,
      "start_column": 12
    },
    "message": "Type Error: Expected object"
  },
  "headers": [
    [
      "content-type",
      "application/json"
    ]
  ],
  "method": "GET",
  "webhook_url": "https://localhost:1234/"
}
```

## Console

The console DX is crucial for making the feature successful. We need a way to test a template with sample data. For this, the console needs to generate sample data (payload) for a given webhook. After this, the console takes the provided template and passes it to debugging endpoint described in the previous section.
