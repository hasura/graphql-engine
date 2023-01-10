# Introduction

We want to translate OpenAPI specifications into GraqhQL to facilitate the creation of actions. The translation process presents several issues; you can explore them in this paper: [https://arxiv.org/abs/1809.08319](https://arxiv.org/abs/1809.08319). 

I find it interesting to point out the following aspects of the translation process:
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

# Tools selection

We need several operations to generate Hasura actions from OpenAPI specifications.

- **parsing** of the OpenAPI specification
- **translating** the OpenAPI specification into GraphQL
- **manipulating** the  GraphQL schema.
  
For the first two points, the choice seems inevitable to fall on the `openapi-to-graphql` library [https://github.com/IBM/openapi-to-graphql](https://github.com/IBM/openapi-to-graphql), created by the authors of the article cited above. 

An alternative might be swagger-to-graphql [https://github.com/yarax/swagger-to-graphql](https://github.com/yarax/swagger-to-graphql). However, this project is much less widely used [https://npmtrends.com/openapi-to-graphql-vs-swagger-to-graphql](https://npmtrends.com/openapi-to-graphql-vs-swagger-to-graphql) and is less robust than the other [https://npmcompare.com/compare/openapi-to-graphql,swagger-to-graphql](https://npmcompare.com/compare/openapi-to-graphql,swagger-to-graphql)

For the third point, the choice fell on `microfiber` [(https://github.com/anvilco/graphql-introspection-tools)](https://github.com/anvilco/graphql-introspection-tools), a tool for manipulating GraphQL schemas. There does not seem to be any other tool that performs this task [https://graphql.org/code/#javascript-tools](https://graphql.org/code/#javascript-tools)

The bundle size (minified + gzipped) for the two tools is 6.2kb for microfiber [https://bundlephobia.com/package/microfiber@1.3.1](https://bundlephobia.com/package/microfiber@1.3.1) and 331kb for openapi-to-graphql [https://bundlephobia.com/package/openapi-to-graphql@2.6.3](https://bundlephobia.com/package/openapi-to-graphql@2.6.3). We could consider checking if tree-shaking is available or maintaining a lighter version ourselves for the latter.

# Openapi to Hasura action

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

## Generate Graphql types

The process of generating Graphql types is the following:
1. Translate the OpenAPI spec into a GraphQL schema using openapi-to-graphql
2. Removing from the schema all the operations but the selected one using microfiber
print the resulting schema
3. dividing action definition (everything withing type Query {} or type Mutation {} ) from type definition (anything else)
4. We should note that we cannot translate all operations in the openapi specification to GraphQL. In this case, the openapi-to-graphql librate will exclude these operations, which will not be among those selectable for translation.

The schema generated by openapi-to-graphql, after point 2, is the following:

```graphql
scalar BigInt

type Category {
  id: BigInt
  name: String
}

input CategoryInput {
  id: BigInt
  name: String
}

type Mutation {
  updatePet(petInput: PetInput!): Pet
}

type Pet {
  category: Category
  id: BigInt
  name: String!
  photoUrls: [String]!
  status: Status
  tags: [Tag]
}

input PetInput {
  category: CategoryInput
  id: BigInt
  name: String!
  photoUrls: [String]!
  status: Status
  tags: [TagInput]
}

type Query

enum Status {
  available
  pending
  sold
}

type Tag {
  id: BigInt
  name: String
}

input TagInput {
  id: BigInt
  name: String
}
```

then, since updatePet is a `POST` operation, the action is defined in the `Mutation` type. We can extract the action definition of the `updatePet` action by removing all the types except `Mutation`.

```graphql
type Mutation {
  updatePet(petInput: PetInput!): Pet
}
```

the remaining are the types used by the action. We can extract them by removing all the types except `Query` and `Mutation`.

## Deal with translation errors

While `openapi-to-graphql` sometimes fails, we could try a best-effort approach to fix those errors in the original specification and then translate it again. The proposal approach is to make some midifications to the OpenAPI specification before translating it to GraphQL. We can have two possible outcomes

- The action is modified in such a way that it can be translated to GraphQL. In this case, we can generate the action.
- The action is discarded so the other actions can be translated.
  
The errors type and corresponding solutions are the following:

### Boolean enum types

GraphQL does not support boolean enum types. Since this is most likely done to restrict the values to only true or false (e.g., in some API the `deleted` response can be only true), we can **replace the enum type with a boolean type**, and translate the action successfully.

### Empty types

GraphQL does not support empty types, which sometimes are present in the OpenAPI specification. We can **change the empty response to another type (e.g. string) or create a fake non-empty object type with a nullable fake field** and translate the action successfully.

###

## Generate Hasura action configuration

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

## Authentication

OpenAPI specification allows to specify authentication methods in the security section and should be managed case by case. In the paper, IBM folks manage authentication through GraphQL Viewers. In our case, for the first release, we will enable the flag `Forward client headers to webhook,` which will probably be enough for most cases (e.g., users can pass the JWT token in the headers).