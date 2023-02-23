---

author:
- Daniele <daniele.cammareri@hasura.io>
- Rahul <rahul.agarwal@hasura.io>

---

# Import REST endpoints as Hasura actions

## Description

The most common use case for Hasura actions is to integrate with a REST endpoint. Import Action from OpenAPI is a feature that lets you easily create Hasura actions from an OpenAPI specification. You can upload the OpenAPI specification and create actions from the endpoints defined in the specification.
For more details on how to use this feature, see the [documentation](https://hasura.io/docs/latest/actions/open-api/).

## Index

- [Import REST endpoints as Hasura actions](#import-rest-endpoints-as-hasura-actions)
  - [Description](#description)
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
    - [Authentication](#authentication)
  - [Future work](#future-work)
    - [Support for other formats](#support-for-other-formats)
    - [Connect a REST API as a source](#connect-a-rest-api-as-a-source)
    - [Bulk Import / Script plugin](#bulk-import--script-plugin)
    - [Import from URL](#import-from-url)
    - [Robust error handling while importing](#robust-error-handling-while-importing)
    - [Action to Action Joins](#action-to-action-joins)

## Motivation

One of the most common use case for Hasura users is to integrate an existing API into the generated GraphQL schema. This is done by creating a Hasura action which integrates with the API through a webhook. This is a very powerful feature and is used by many users. However, creating an action can be a bit tedious since there are a few things to set up:

- Create input and output types for the action
- Configure the webhook calls, headers, parameters, payload, etc.
- Configure request and response transformations

There are several APIs format available in the market, which describe the API in a standard format. For example, OpenAPI, gRPC, etc. These formats are used by many API providers to describe their APIs. It would be great if Hasura can import these API descriptions and automatically create actions for them.

Among the many API formats, OpenAPI is the most popular and widely used. It is used by many API providers to describe their APIs. It is also used by many API consumers to generate client libraries for the APIs. Is is also used as a documentation format for APIs which can be imported in tools like Postman.
It would be great if Hasura can import OpenAPI descriptions and automatically create actions for them.

We identified two main use cases for this feature:

- Users who develop their own APIs and want to integrate them with Hasura
- Users who want to integrate an existing API into Hasura (e.g. Stripe, Twilio, etc.), when a graphql version is not available (in that case Remote schemas can be used with a better integration and permission system)

Our users identified several pain points with the current approach of creating actions:

- Some of them need to add 25 to 500 distinct REST APIs to their Hasura instance. This is a tedious process and requires a lot of manual work.
- They need an easy way to integrate with an existing API (e.g, Stripe or Twilio)


## Solution

The idea is to provide a way to automate the process of creating actions for REST endpoints by importing an OpenAPI description. In the same way client libraries are generated from OpenAPI descriptions, we can generate actions from OpenAPI descriptions. We identified two main approaches for this:

- Create an action for each endpoint in the OpenAPI description. Once the action is created, the relationship with the original API is lost and if the user need to update the action, they need to update the action definition. 
- Connect an OpenAPI description to a Hasura source, similar to how Remote Schemas works for GraphQL external APIs. This would affect the Hasura metadata and keep the connection with the original API. If the API changes, the metadata can be updated to reflect the changes.

We decided to start with the first approach, since it is simpler and can be used by users who want to integrate an existing API into Hasura. The second approach can be implemented later.

We used the petstore example from the [Petstore OpenAPI specification](https://petstore3.swagger.io/) as a reference to identify the different types of endpoints that can be generated from an OpenAPI description.

We decided to split the release into three phases:

- Phase 1 (alpha): we support only the enpoints from the petstore example which can directly be converted to actions given the current implementation. Some endpoint from the Petstore example, such as the `GET /pet/findByTags` and `POST /pet/{petId}/uploadImage` endpoints, who lack of support for lists in query parameters and file uploads, will be supported in the next phases. We create a basic UI to quickly release and start gathering feedback from users.

- Phase 2 (GA): we support all the endpoints from the petstore example. This includes endpoints with lists in query parameters and (potentially) file uploads. We also improve the UI to make it easier to use. We also analyze the feedback from the alpha release and make improvements based on that.

## Implementation

For translating  OpenAPI specifications into Hasura actions, we used the implementation of the authors of the following paper: [https://arxiv.org/abs/1809.08319](https://arxiv.org/abs/1809.08319). 

The authors of the paper propose a solution to translate OpenAPI specifications into GraphQL schemas. The two main issues they identified are the following:

- **deduplication of input type names**: in GraphQL, it is convenient to have shared input types across operations, but in an OpenAPI specification, you could define the same type multiple times across operations. The proposed approach is to deduplicate input types by making a deep comparison between them.
- **sanitization of names**: In GraphQL, names must follow the following regular expression n /[_A-Za-z][_0-9A-Za-z]*/, whereas in OpenAPI this is not the case. The proposed approach uses a name sanitization function, which is important to consider when implementing the call translation process. In our case, the configuration of the actions

The authors of the paper also tested their implementation (the one we used) against many OpenAPI specifications, and they found those results:

- 97% of the OpenAPI specifications can be translated into GraphQL
- 27.1% of the OpenAPI specifications can be translated into GraphQL when strict mode is enabled (without any warning)

The errors/warings they found are the following:

- **Name sanitization errors**: mostly due to the translation of boolean enum values (true and false are not valid GraphQL enum values)
- **Invalid OAS**:  the input OpenAPI specification could not be successfully validated.`
- **Missing ref**: the input OpenAPI specification contains a reference to a schema that could not be found.

In addition, I will report the most common problems that we found when translating OpenAPI operations to Hasura Actions:

- **Wrong response type**: the response type in the OpenAPI specification is missing or is wrong. An additional problem is when the success response is defined as `default` instead of `200`.
- **Missing operations**: operations that cannot be translated are missing in the translation output.
- **Failing OpenAPI specification**: a few number of OpenAPI specifications are not valid, and they cannot be translated at all.
- **Forms**: some operations require the input to be passed as a form, which is not supported by Hasura Actions.
- **OpenAPI 2.0 support**: the OpenAPI 2.0 specifications must be translated to OpenAPI 3.0 before being translated to GraphQL. This sometimes leads to errors in the translation process.
- **Array query Params**: when array inputs must be serialized as query parameters, there isn't a kriti function that let you to do that

In this document, we explore our translation proposal directly into the console. We want to create a tool for importing a single action. The input is the GraphQL specification and the operation's name to translate, and the result is the complete pre-compilation of the action creation form.

### Tools selection

We need several operations to generate Hasura actions from OpenAPI specifications.

- **parsing** of the OpenAPI specification
- **translating** the OpenAPI specification into GraphQL
- **manipulating** the  GraphQL schema.
  
For the first two points, the choice seems inevitable to fall on the `openapi-to-graphql` library [https://github.com/IBM/openapi-to-graphql](https://github.com/IBM/openapi-to-graphql), created by the authors of the article cited above. 

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
  - the parameters to pass in the query string\
  - the response transformation

As an example, we will simulate the generation of the `updatePet` action of the [https://petstore3.swagger.io/api/v3/openapi.json](https://petstore3.swagger.io/api/v3/openapi.json) specification.

#### Generate Graphql types

The process of generating Graphql types is the following:
1. Translate the OpenAPI spec into a GraphQL schema using openapi-to-graphql
2. Removing from the schema all the operations but the selected one using microfiber
print the resulting schema
3. dividing action definition (everything withing type Query {} or type Mutation {} ) from type definition (anything else)
4. We should note that we cannot translate all operations in the openapi specification to GraphQL. In this case, the openapi-to-graphql librate will exclude these operations, which will not be among those selectable for translation.

#### Deal with translation errors

While `openapi-to-graphql` sometimes fails, we could try a best-effort approach to fix those errors in the original specification and then translate it again. The proposal approach is to make some midifications to the OpenAPI specification before translating it to GraphQL. We can have two possible outcomes

- The action is modified in such a way that it can be translated to GraphQL. In this case, we can generate the action.
- The action is discarded so the other actions can be translated.
  
The errors type and corresponding solutions are the following:

##### Boolean enum types

GraphQL does not support boolean enum types. Since this is most likely done to restrict the values to only true or false (e.g., in some API the `deleted` response can be only true), we can **replace the enum type with a boolean type**, and translate the action successfully.

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

If there were a one-to-one relationship between REST and GraphQL types, there would be no need for any request or response transformation. But, as is stated in the IBM article, to generate GrahpQL types,  some names could be sanitized and hence be different from the REST ones. This could lead to broken Hasura action calls.

To solve this problem, a layer of request and response transformation is needed to perform the translation of types between the REST and GraphQL worlds.

While in the article this is is done in the generated resolvers, in Hasura action kriti templates must be generated by recursively traversing the GraphQL schema and the OpenAPI specification and used as request and response transformation. 

This is an example of `PetInput` request and response kriti transformation. We artificially renamed the `name` field to in OpenAPI spefication to `$name` to simulate the incompatibility.

```json
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

```json
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

### Authentication

OpenAPI specification allows to specify authentication methods in the security section and should be managed case by case. In the paper, IBM folks manage authentication through GraphQL Viewers. In our case, for the first release, we will enable the flag `Forward client headers to webhook,` which will probably be enough for most cases (e.g., users can pass the JWT token in the headers).

## Future work

### Support for other formats

The OpenAPI specification is not the only format to describe REST APIs. There are other formats like gRPC, AsyncAPI, etc. We could support them in the future. We could also add support to 3.1 version of the OpenAPI specification.

### Connect a REST API as a source

We could also support the connection of a REST API as a source. This would create an entry for the connected API in the server metadata and allow to use it as a source in the Hasura console, so that the connection with the original API is maintened and the changes are propagated to Hasura.

### Bulk Import / Script plugin

We could also support the bulk import of APIs. This would allow to import a list of APIs in a single operation. This could be done in two ways:

- A CLI script plugin that allows to import a list of APIs in a single operation
- A bulk import feature in the Hasura console

### Import from URL

We could also support the import of APIs from a URL as an additional option to the copy and paste or file upload. CORS should be taken into account.

### Robust error handling while importing

As stated in the paper by IBM, several errors can occur while importing an API. We should handle them in a robust way to help the user to identify and potentially fix the errors. We could also improve the automatic error handling by adding some heuristics to modify the OpenAPI specification before translating it to GraphQL.

### Action to Action Joins

When we'll introduce the concept of action to action joins, we could also support the import of APIs that are connected to each other, opening the possibility to create relationships between imported actions.
