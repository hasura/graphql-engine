# Roadmap Proposal

| Version | MVP                                                                                                                        | Functionality                               | Notes / Comments                            |
|---------|----------------------------------------------------------------------------------------------------------------------------|---------------------------------------------|---------------------------------------------|
| V1      | Bare minimum useful functionality                                                                                          | List of endpoints with methods and comments | **Completed**                               |
| V2      | Bare minimum useful functionality                                                                                          | All request types / arguments documented    | **Completed**                               |
| V3      | User will get the Swagger JSON blob that can be used at [Swagger editor](https://editor.swagger.io/) to get the swagger UI | All response types documented               | **Completed**                               |
| V4      | The Swagger UI will support authorization via header                                                                       | Support role based authorization system     | **Completed**                               |
| V5      | The swagger UI can be accessed using the Hasura console                                                                    | UI in console                               | (We may not implement this)                 |


# Overview

OpenAPI provides a common framework for designers, developers, testers, and devops to build and maintain REST APIs. We aim to generate a specification page that documents the REST endpoints and provides a tool to execute them with appropriate parameters. The consumers can understand and connect with the API remote services using the UI.

This document will specify the integration of REST endpoints that Hasura uses/generates with the OpenAPI specifications and generate json accepted by OpenAPI standards to design, build, document, test and standardize REST API endpoints.


# Why integrate OpenAPI?

* Helps in collaborative API development
* OpenAPI definitions can be converted to code using tools like swagger-codegen
* Provides machine-readable definitions, providing easy testing
* Generates interactive documentation integrated with test UI clients


# What do we need to generate the swagger specification?

* API endpoint (URL)
* Method of the API (POST/GET)
* Parameters required (query variables in our case)
* Request body (GQL variables)
* Response body (can be generated if we know the query and data types)

# How will the user access the openAPI documentation?

* User will go to api/swagger/json
* They will get the swagger specification (JSON/YAML), that may be rendered as the swagger UI ([an example](https://petstore.swagger.io/))
* The user will be asked to provide some authentication token (can either be x-hasura-admin-secret or JWT) in swagger UI to access the endpoint

![User Flow](https://user-images.githubusercontent.com/92299/178194103-8789fee3-5e0e-4b1e-a387-3cff6702f753.png)


# What will happen in the HGE?

To tackle this, we will:

Generate the OpenAPI specification when the user hits the endpoint (api/swagger/json). This will dynamically generate the JSON blob based on the current role of the user. We need to create the OpenAPI specification JSON blob using the REST endpoint specification. The REST endpoint specification can be extracted from either the schema cache, schema cache would be a good option for this (precomputing when dependencies change, more information about any issues upfront rather than at query-time).

Following is the roadmap for extracting the OpenAPI specification from the schema cache:

* First, extract the relevant information from the schema cache. The endpoints are stored as triemap in the schema cache, which is a list of endpoints, a typical endpoint data stored in the list is: \

``` JSON
[
    {
        "tag": "PathLiteral",
        "contents": "myAPIpost"
    },
    {
        "_trieData": {
            "POST": [
                {
                    "definition": {
                        "query": "mutation MyMutation($col_1: String = \"\", $col_2: String = \"\", $id: Int = 10) {\n  insert_table_1(objects: {col_1: $col_1, col_2: $col_2, id: $id}) {\n    affected_rows\n  }\n}"
                    },
                    "url": "myAPIpost",
                    "methods": [
                        "POST"
                    ],
                    "name": "newAPI",
                    "comment": null
                }
            ]
        },
        "_trieMap": []
    }
]
```


* The API endpoint can be extracted from the url field and the allowed method from the methods list, the parameters can be extracted from the query by parsing the GQL query (?) and the response body can also be generated from the GQL query (?).
* Then the JSON blob of the endpoints in OpenAPI specification can be sent to the user (or we can serve the json in a swagger UI as well)

# How are we going to serve the swagger UI - Proposal

1. Expose the Swagger JSON directly for the user. We can add a button in the console where user can download the JSON and then they can use Swagger editor of their choice (eg. https://editor.swagger.io/).

2. Serve the Swagger UI at an endpoint (`/api/swagger`) using a CDN (eg. https://www.jsdelivr.com/package/npm/swagger-ui-dist).

3. Create our own custom Swagger UI using the OpenAPI JSON (refer to https://github.com/swagger-api/swagger-ui).

| Option | Pros | Cons |
|--------|------|------|
| 1      |  User will have full control over the UI  |   Not very user friendly   |
| 2      |  User will get the familiar Swagger UI    |   Will expose our system to security threats from external CDN  |
| 3      |  We will have full control over the UI, scalable, we can re-use telemetry system, possibly we could re-use the locally stored headers from GraphiQL   |   Have to create our own UI , bundle size might increase (code splitting might help)  |

> We can also embed the Swagger UI (editor.swagger.io) in an `iFrame`

> We can serve the static Swagger files without a CDN and use them to render the OpenAPI JSON directly.

An example of serving Swagger UI using CDN (Option 2)
```html
<!DOCTYPE html>
 <html lang='en'>
 <head> 
     <meta charset='UTF-8'> 
     <meta name='viewport' content='width=device-width, initial-scale=1.0'> 
     <meta http-equiv='X-UA-Compatible' content='ie=edge'> 
     <script src='https://cdnjs.cloudflare.com/ajax/libs/swagger-ui/3.22.1/swagger-ui-standalone-preset.js'></script> 
     <script src='https://cdnjs.cloudflare.com/ajax/libs/swagger-ui/3.22.1/swagger-ui-bundle.js'></script> 
     <link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/swagger-ui/3.22.1/swagger-ui.css' /> 
     <title>Swagger</title> 
 </head> 
 <body> 
     <div id='swagger-ui'></div> 
     <script> 
         window.onload = function() { 
           SwaggerUIBundle({ 
             url: '/api/swagger/json', 
             dom_id: '#swagger-ui', 
             presets: [ 
               SwaggerUIBundle.presets.apis, 
               SwaggerUIStandalonePreset 
             ], 
             layout: 'StandaloneLayout' 
           }) 
         } 
     </script> 
 </body> 
 </html>
```

Currently we are simply exposing the JSON as per options 1.


# How to generate types from query representation?

The query parser that is being used resolves the query, a subsequent method statically analyzes the type of each field present in the query by leveraging the introspection schema.

# Future Work/ Open Ended Questions

* How to handle authentication in openAPI interative UI?
* Should we expose different information based on the role of the user?
