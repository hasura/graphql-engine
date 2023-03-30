---

author:
- Daniele <daniele.cammareri@hasura.io>
- Rahul <rahul.agarwal@hasura.io>

---

# Import OpenAPI Specification (OAS) into Hasura

This RFC proposes a way to import REST endpoints into Hasura GraphQL Engine via Hasura actions. This allows users to create a unified API across varied data sources such as databases, GraphQL APIs and REST APIs seamlessly.
For more details on how to use this feature, see the [documentation](https://hasura.io/docs/latest/actions/open-api/).

## Index

- [Import OpenAPI Specification (OAS) into Hasura](#import-openapi-specification-oas-into-hasura)
  - [Index](#index)
  - [Motivation](#motivation)
  - [Solution](#solution)
  - [Implementation](#implementation)
    - [Tools selection](#tools-selection)
    - [Openapi to Hasura action](#openapi-to-hasura-action)
      - [Generate Graphql types](#generate-graphql-types)
      - [Deal with translation errors](#deal-with-translation-errors)
        - [Boolean enum types](#boolean-enum-types)
        - [Empty types](#empty-types)
    - [Generate Hasura action configuration](#generate-hasura-action-configuration)
    - [Request and response transormation](#request-and-response-transormation)
    - [API Authentication](#api-authentication)
  - [Future work](#future-work)
    - [Support for OAS 3.1](#support-for-oas-31)
    - [Support for other API formats](#support-for-other-api-formats)
    - [Connect an OAS based APIas a data source](#connect-an-oas-based-apias-a-data-source)
    - [Bulk Import / Script plugin](#bulk-import--script-plugin)
    - [Action Management](#action-management)
    - [Robust error handling while importing](#robust-error-handling-while-importing)
    - [REST API to REST API Joins](#rest-api-to-rest-api-joins)
    - [Union Type Support for Actions](#union-type-support-for-actions)
    - [URL Form Encoded](#url-form-encoded)

## Motivation

We have two sets of users looking to import OAS into Hasura. First, we have the platform teams trying to integrate REST Endpoints into one unified GraphQL API. They have anywhere from 25 to 500 distinct REST APIs with up to 100 endpoints per API and look for an automated way to bring in OAS based APIs. Second, we have the application team using solutions and templates to build applications. They need quick plug and play with standard known OAS based APIs such as Twilio and Stripe.

The current way to import a REST endpoint into Hasura is via creating an Action which integrates with the API through a webhook. This is a highly used  feature of our platform but creating an action for ingesting a REST endpoint can gettedious since there are a few things to set up:
 
 - Create input and output types for the GraphQL API by first converting JSON based schema to GraphQL Schema Definition Language
 - Configure the webhook methods, headers, parameters, payload and URL templates.
 - Configure request and response body transformations
  
There are several APIs formats available in the market, which describe the API in a standard format. For example, OpenAPI, gRPC, etc. These formats are used by many API providers to describe their APIs. It would benefit the application and platform teams if they could seamlessly bring in API endpoints in any format into their unified GraphQL schema for their API consumers. 

Among the many API formats, OpenAPI is the most popular and widely used. It is used by many API providers to describe their APIs. It is also used by many API consumers to generate client libraries for the APIs. It is also used as a documentation format for APIs which can be imported in tools like Postman and visualized via SwaggerUI. We decided to automate OAS ingestion first since it has become the de-facto standard for APIs in enterprises and will help them modernize their technology stack using Hasura’s GraphQL solution.

## Solution

The idea is to provide a way to automate the process of creating actions for REST endpoints by importing an OpenAPI description. In the same way client libraries are generated from OpenAPI descriptions, we can generate actions from OpenAPI descriptions. We identified two main approaches for this:

- Create an action for each endpoint in the OpenAPI specification. Once the action is created, the relationship with the original API is lost and if the user needs to update the action, they need to update the action definition. This approach has a larger impact on the metadata as it contains the definition of each endpoint represented as an Action.
- Connect an OpenAPI specification to  Hasura via URL similar to how Remote Schemas works for GraphQL external APIs. This would have the least affect on the Hasura metadata since the schema will be introspected at periodic intervals. As a result,  if the upstream API changes, the GraphQL schema can be automatically updated to reflect the changes.

We decided with the first approach, since it can leverage existing functionality, while the second approach requires a larger platform change and is captured in the future enhancements section of this RFC. We used the petstore example from the [Petstore OpenAPI specification](https://petstore3.swagger.io/) as a reference to identify the different types of endpoints that can be generated from an OpenAPI description.
We decided to split the release into three phases:

- **Phase 1 (Alpha):** We supported five types of  endpoints from the petstore example which can directly be converted to actions given the current implementation. Some endpoints from the Petstore example were not supported in this iteration, such as the GET /pet/findByTags and POST /pet/{petId}/uploadImage endpoints, which lack support for lists in query parameters and file uploadsWe created a basic UI to quickly release and start gathering feedback from users. The two main mechanisms to import the API would be to upload a file or pasting the contents of the file in the JSON editor form on the UI.

- **Phase 2 (GA):** We supported seven types of  endpoints from the petstore example except the file upload endpoint. The new additions include endpoints with lists in query parameters.. We also provided a comprehensive UI that made it easier to navigate large openAPI specs and import them in bulk easily. We added error handling mechanisms wherever required. We also tested for different types of OAS files other than petstore and made improvements to increase the accuracy rate of REST to GraphQL endpoint conversion.
## Implementation

For translating OpenAPI specifications into Hasura actions, we used the implementation of the authors of the following paper: [https://arxiv.org/abs/1809.08319](https://arxiv.org/abs/1809.08319). 

The authors of the paper propose a solution to translate OpenAPI specifications into GraphQL schemas. The two main issues they identified are the following:

- **deduplication of input type names**: in GraphQL, it is convenient to have shared input types across operations, but in an OpenAPI specification, you could define the same type multiple times across operations. The proposed approach is to deduplicate input types by making a deep comparison between them.
- **sanitization of names**: In GraphQL, names must follow the following regular expression n /[_A-Za-z][_0-9A-Za-z]*/, whereas in OpenAPI this is not the case. The proposed approach uses a name sanitization function, which is important to consider when implementing the call translation process. In our case, the configuration of Hasura actions

The authors of the paper also tested their implementation (the one we used) against many OpenAPI specifications, and they found those results:

- 97% of the OpenAPI specifications can be translated into GraphQL
- 27.1% of the OpenAPI specifications can be translated into GraphQL when strict mode is enabled (without any warning)

The errors/warnings they found are the following:

- **Name sanitization errors**: mostly due to the translation of boolean enum values (true and false are not valid GraphQL enum values)
- **Invalid OAS**: the input OpenAPI specification could not be successfully validated.
- **Missing ref**: the input OpenAPI specification contains a reference to a schema that could not be found.

In addition, following are the most common problems that we found when translating OpenAPI endpoints to Hasura Actions:

- **Wrong response type:** the response type in the OpenAPI specification is missing or is wrong. An additional problem is when the success response is defined as default instead of 200.
- **Missing operations:** operations that cannot be translated are missing in the translation output.
- **Failing OpenAPI specification:** a few OpenAPI specifications are not valid, and they cannot be translated at all.
- **Forms:** some operations require the input to be passed as a form, which is not supported by Hasura Actions.
OpenAPI 2.0 support: the OpenAPI 2.0 specifications must be translated to OpenAPI 3.0 before being translated to GraphQL. This sometimes leads to errors in the translation process.
- **Array query Params:** when array inputs must be serialized as query parameters, there isn't a kriti-lang function that lets you to do that.
  
### Tools selection

We need several operations to generate Hasura actions from OpenAPI specifications.

- **parsing** of the OpenAPI specification
- **translating** the OpenAPI specification into GraphQL
- **manipulating** the  GraphQL schema.
  
For the first two points, the choice seems inevitable to fall on the [openapi-to-graphql library](https://github.com/IBM/openapi-to-graphql), created by the authors of the article cited above.

An alternative might be swagger-to-graphql [https://github.com/yarax/swagger-to-graphql](https://github.com/yarax/swagger-to-graphql). However, this project is much less widely used [https://npmtrends.com/openapi-to-graphql-vs-swagger-to-graphql](https://npmtrends.com/openapi-to-graphql-vs-swagger-to-graphql) and is less robust than the other [https://npmcompare.com/compare/openapi-to-graphql,swagger-to-graphql](https://npmcompare.com/compare/openapi-to-graphql,swagger-to-graphql)

For the third point, the choice fell on `microfiber` [(https://github.com/anvilco/graphql-introspection-tools)](https://github.com/anvilco/graphql-introspection-tools), a tool for manipulating GraphQL schemas. There does not seem to be any other tool that performs this task [https://graphql.org/code/#javascript-tools](https://graphql.org/code/#javascript-tools)

The bundle size (minified + gzipped) for the two tools is 6.2kb for microfiber [https://bundlephobia.com/package/microfiber@1.3.1](https://bundlephobia.com/package/microfiber@1.3.1) and 331kb for openapi-to-graphql [https://bundlephobia.com/package/openapi-to-graphql@2.6.3](https://bundlephobia.com/package/openapi-to-graphql@2.6.3). We could consider checking if tree-shaking is available or maintaining a lighter version ourselves for the latter.

### Openapi to Hasura action

To achieve complete action, we need to:

- Generate the GraphQL types:
  - for the action
  - for the types used by the action
- Generate the action configuration:
  - the payload of the request
  - the http method
  - the path parameters of the URL
  - the parameters to pass in the query string
  - the response transformation

As an example, we will simulate the generation of the `updatePet` action of the [Petstore OpenAPI specification](https://petstore3.swagger.io/) specification.

#### Generate Graphql types

The process of generating Graphql types is the following:

1. Translate the OpenAPI spec into a GraphQL schema using openapi-to-graphql
2. Removing from the schema all the operations but the selected one using microfiber print the resulting schema
3. dividing action definition (everything within type `Query {}` or type `Mutation {}` ) from type definition (anything else)
4. We should note that we cannot translate all operations in the openapi specification to GraphQL. In this case, the openapi-to-graphql librate will exclude these operations, which will not be among those selectable for translation.
#### Deal with translation errors

While `openapi-to-graphql` sometimes fails, we could try a best-effort approach to fix those errors in the original specification and then translate it again. The proposal approach is to make some modifications to the OpenAPI specification before translating it to GraphQL. We can have two possible outcomes

- The action is modified in such a way that it can be translated to GraphQL. In this case, we can generate the action.
- The action is discarded so the other actions can be translated.
  
The errors type and corresponding solutions are the following:

##### Boolean enum types
GraphQL does not support boolean enum types. Since this is most likely done to restrict the values to only true or false (e.g., in some API the deleted response can be only true), we can **replace the enum type with a boolean type**, and translate the action successfully.

##### Empty types

GraphQL does not support empty types, which sometimes are present in the OpenAPI specification. We can **change the empty response to another type (e.g. string) or create a fake non-empty object type with a nullable fake field** and translate the action successfully.

### Generate Hasura action configuration

We can derive all the Hasura action configurations by the openapi-to-graphql metadata for the selected operation in a straightforward way.

  - the **request and response transformations** are discussed in the section below.
  - the **http method** is got directly from the openapi-to-graphql metadata
  - the **base URL** of the API is got from the server section of the openapi specification
  - the **operation URL** is the path of the operation in the metadata
  - the **path parameters** of the URL is the list of arguments that are marked as path parameters in the metadata
  - the parameters to pass in the **query string**: is the list of arguments that are marked as query parameters in the metadata
### Request and response transormation

If there were a one-to-one relationship between REST and GraphQL types, there would be no need for any request or response transformation. But, as is stated in the IBM article, to generate GraphQL types, some names could be sanitized and hence be different from the REST ones. This could lead to broken Hasura action calls.

To solve this problem, a layer of request and response transformation is needed to perform the translation of types between the REST and GraphQL worlds.

While in the article this is done in the generated resolvers, in Hasura action kriti templates must be generated by recursively traversing the GraphQL schema and the OpenAPI specification and used as request and response transformation.

This is an example of `PetInput` request and response kriti transformation. We artificially renamed the `name` field to in OpenAPI spefication to `$name` to simulate the incompatibility.

```
{
  "id": {{$body.input.petInput?.id}},
  "$name": {{$body.input.petInput?.name}},
  "category": {
    "id": {{$body.input.petInput?.category?.id}},
    "name": {{$body.input.petInput?.category?.name}}
  },
  "photoUrls": {{$body.input.petInput?.photoUrls}},
  "tags": {{if inverse(empty($body.input.petInput?.tags))}} {{ range _, tags := $body.input.petInput?.tags}} {
    "id": {{tags?.id}},
    "name": {{tags?.name}}
    } {{end}} {{else}} null {{end}},
  "status": {{$body.input.petInput?.status}}
}
```

and the opposite:

```
{
  "id": {{$body?.id}},
  "name": {{$body?.$name}},
  "category": {
    "id": {{$body?.category?.id}},
    "name": {{$body?.category?.name}}
  },
  "photoUrls": {{$body?.photoUrls}},
  "tags": {{if inverse(empty($body?.tags))}} {{ range _, tags := $body?.tags}} {
    "id": {{tags?.id}},
    "name": {{tags?.name}}
    } {{end}} {{else}} null {{end}},
  "status": {{$body?.status}}
}
```

note how objects and arrays are handled.

### API Authentication

OpenAPI specification allows to specify authentication methods in the security section and should be managed case by case. In the paper, IBM folks manage authentication through GraphQL Viewers. In our case, for the first release, we will enable the flag Forward client headers to webhook, which will probably be enough for most cases (e.g., users can pass the JWT token in the headers).

## Future work

### Support for OAS 3.1

Widen the support of OAS import to the 3.1 version. Currently only OAS 3.0 is supported. While implementing this we need to consider the support of JSON Schema as well. 
### Support for other API formats

The OpenAPI specification is not the only format to describe REST APIs. There are other formats such as  gRPC and AsyncAPI. We could support them in the future. 

### Connect an OAS based APIas a data source

We could also support the connection of a REST API as a source via a URL. This would create an entry for the connected API in the server metadata and allow it to use it as a source in the Hasura console, so that the connection with the original API is maintained and the changes are propagated to Hasura. Other consideration to keep in mind when implemnting this include 1) CORS permission issue and 2) Head method - introspection

### Bulk Import / Script plugin

We could also support the bulk import of APIs. This would allow importing  a list of APIs in a single operation. This could be done in two ways:

- A CLI script plugin that allows to import a list of APIs in a single operation
- A bulk import feature in the Hasura console
### Action Management

As we enable quick and easy creation of Actions from potentially 100s of REST endpoints there are opportunities for us to improve handling of Actions on the platform in some of the following ways:

- Providing a state of draft actions before creating so as to provide a way for user to conduct quality checks.
- Grouping of Actions either via the API baseURL or custom tags.
### Robust error handling while importing

As stated in the paper by IBM, several errors can occur while importing an API. We should handle them in a robust way to help the user to identify and potentially fix the errors. We could also improve the automatic error handling by adding some heuristics to modify the OpenAPI specification before translating it to GraphQL. Also during bulk importing, we could provide detailed error statements about which Actions failed and the ability to modify them to try again.

One other type of errors that occur with importing are around type resolution. Creating types for an OAS endpoint that might conflict or overlap with custom types that already exist in actions, or types that exist from other data sources like databases and remote schemas.

### REST API to REST API Joins

The ability to query data spread across different upstream REST APIs via a single GraphQL call.  When we introduce the concept of REST to REST joins, we could also support the import of APIs that are connected to each other, opening the possibility to create relationships between imported REST endpoints. More info in [this Github issue](https://github.com/hasura/graphql-engine/issues/8015).

### Union Type Support for Actions

Unions are a fundamental way of representing ADTs (algebraic data types) and are currently not supported in the Hasura GraphQL Engine and hence such types cannot be imported into Hasura. More info in the Github Issue here.

### URL Form Encoded

Currently, import from OAS only supports JSON request bodies. We could also support URL Form Encoded request bodies, which is required for some APIs like Twilio. Hasura Action already supports URL Form Encoded request bodies, so this would be a matter of adding support for it in the OAS import.