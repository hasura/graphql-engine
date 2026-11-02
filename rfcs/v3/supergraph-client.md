# Supergraph Client RFC

Customers have a few use cases that could be served by enabling certain types of connectors (code-based connectors, such as the NodeJS Lambda Connector) to issue requests back into the Hasura Supergraph at runtime:

* I want to issue multiple GraphQL queries/mutations and interleave business logic amongst them
* I want to apply validation logic before an existing mutation in the supergraph
* I want to run complex authorization logic before applying a mutation
* I want to run a saga/workflow of business logic interleaved with queries/mutations

By allowing code-based connectors to call back into the Supergraph, customers could address these use cases by simply exposing new Open DD commands that are backed by code that composes together queries to the Supergraph as necessary.

Currently connectors sit downstream from the v3-engine, which exposes the entire Supergraph as a GraphQL API. Connectors yield data upstream to the engine, but have no facility to call back to the engine.

Theoretically any code connector could call back into the Supergraph right now just by importing a GraphQL client library and talking to the Supergraph like any other API client. However, there are three main issues that make this difficult:

* Addressing: how does the connector get the URL to the Supergraph?
* Authentication/Authorization: How does the connector authenticate to the Supergraph, and what controls what parts of the Supergraph it has access to?
* Supergraph Schema Building: How does the connector acquire the GraphQL schema of the Supergraph?

## Addressing
Connectors are deployed seperately to the Supergraph, which means that the URL to the Supergraph is not available at deployment time. This is particularly true when you consider that many Supergraph builds can reuse the same deployed connector, yet one would wish for the connector to call back into the same Supergraph build that caused the connector to invoked in the first place.

In practice this means that the v3-engine call to the connector would need to provide the connector with the address to the Supergraph instance that is invoking it. This could be achieved via argument binding in the `DataConnectorLink` in Open DD, in a similar fashion to how header passthrough is performed.

```yaml
kind: DataConnectorLink
version: v1
definition:
  url: ...
  schema: ...
  argumentPresets:
    - argumentName: supergraphClient
      value:
        supergraphClient:
          type: GraphQL # Only option for now, maybe in the future we could offer other types, eg REST
```

The connector itself would expose NDC functions/procedures that would accept a SupergraphClient scalar type of JSON representation:

```yaml
scalar_types:
  SupergraphClient:
    representation: json
  Bool:
    representation: boolean
procedures:
  name: myFunction
  arguments:
    supergraphClient:
      type:
        named: SupergraphClient
  resultType:
    named: Bool
```

The engine would send a JSON object with the URL to the current Supergraph build's GraphQL API (when running locally, this would just be a localhost engine URL):

```json
{ "supergraphUrl": "https://settling-impala-7722-50bb46e1d5.ddn.hasura.app/graphql" }
```

To the customer writing code in the code-based connector, the experience could look _something_ like this (example adapted from [gql.tada](https://gql-tada.0no.co/get-started/writing-graphql)):

```typescript
/** @readonly */
export async function myFunction(supergraphClient: sdk.SupergraphClient): Promise<boolean> {
  // Maybe configure auth on the supergraphClient (see next section)
  supergraphClient.defaultHeaders = {
    Authorization: "Bearer ey..."
  };

  // Send a request to the supergraph
  const [result] = await supergraphClient.send({
    query: PokemonsQuery,
    variables: { limit: 5 }
  });

  return result.data?.pokemons?.length > 0;
}

const PokemonsQuery = graphql(`
  query PokemonsList($limit: Int = 10) {
    pokemons(limit: $limit) {
      id
      name
    }
  }
`);
```

## Authentication/Authorization
There are a few different ways we could address the connector authenticating with the Supergraph. These different approaches are not mutually exclusive and all could potentially be implemented to address different customer requirements.

### Leave it up to the customer
In Hasura, we outsource authentication to the customer by getting them to either configure a webhook that we call, or to have their own JWT issuer and configuring Hasura to validate their JWTs. Whatever they do to authenticate to their Supergraph from API client code now, they could simply do in their connector code. For example, they could write code to acquire a JWT from their token server and apply it to the GraphQL request.

All we'd need to ensure is that the Supergraph Client code allowed them to set request headers on outgoing requests.

### Authorization header passthrough
Customers may want the connector to have the same access credentials as the request that caused the connector to be invoked. This could be achieved through the existing header passthrough configuration.

```yaml
kind: DataConnectorLink
version: v1
definition:
  url: ...
  schema: ...
  argumentPresets:
    - argumentName: authHeader
      value:
        httpHeaders:
          forward:
            - Authorization
```

The customer could then write a function that took the `authHeader` argument, and in the function code, they could use the Authorization header value to set the Authorization header on the Supergraph Client requests back to the Supergraph.

### Connector role grants
The above two methodologies require the customer to write explict code in their function to handle authentication with the Supergraph.

When we leave handling authentication up to them, they will likely need to set up an identity for the connector in their JWT provider that the connector can manually acquire a token for, pass a secret to the connector as an env var configuration (eg. a client credentials grant secret), then write code to use that identity to get a token to use with the Supergraph Client.

When using the Authorization header passthrough, the connector is limited to the access rights of the incoming requests, which may not be suitable depending on their use case. The connector may require _more_ rights than the incoming request.

To simplify things for customers, we could offer a way of the customer being able to assign a role to connectors/functions/procedures in their Open DD metadata and the connector then being able to access the Supergraph under that role. This could be facilitated by extending the `supergraphClient` argument preset with the ability to set a role and session variables:

```yaml
kind: DataConnectorLink
version: v1
definition:
  url: ...
  schema: ...
  argumentPresets:
    - argumentName: supergraphClient
      value:
        supergraphClient:
          type: GraphQL
          authorization:
            role: manager
            sessionVariables:
              x-hasura-user-id: 1234
```

The engine would then generate a JWT signed by Hasura internal services (how to do this exactly is TBD) which would be sent as a part of the `supergraphClient` JSON object that would authorize the bearer access to the Supergraph using the specified role and session variable values:

```json
{
  "supergraphUrl": "https://settling-impala-7722-50bb46e1d5.ddn.hasura.app/graphql" ,
  "headers": {
    "Authorization": "Bearer ey..."
  }
}
```

For a locally-running v3-engine using the dev auth hook, the headers would be:

```json
{
  "supergraphUrl": "https://localhost:3000/graphql" ,
  "headers": {
    "x-hasura-role": "manager",
    "x-hasura-user-id": "1234"
  }
}
```

The Supergraph Client code in the connector SDK could automatically pick up the `headers` property and configure the client to automatically send those headers in requests. This would mean the connector would automatically be authenticated to talk back to the Supergraph with no extra code needing to be written in the connector code itself.

## Supergraph Schema Building
In order to generate a GraphQL client in the connector code, the GraphQL client library (whichever we choose) will require the GraphQL introspection schema from the Supergraph. There are couple of ways we could achieve this.

### Introspect a running Supergraph API
This will require a running v3-engine either in the cloud or locally, so that the tooling can issue introspection queries to the GraphQL API.

Imagine a connector CLI plugin that would introspect the GraphQL API and generate the necessary Supergraph Client code:

```
> ddn connector plugin --connector ./connector.yaml -- generate-supergraph-client --url http://localhost:3000/graphql
```

This CLI plugin could use something like [gql.tada to download the schema](https://gql-tada.0no.co/get-started/workflows#downloading-schemas), ready for tooling use.

The only difficult part is authenticating with the specified GraphQL API. We could allow the user to set headers alongside the API URL. Using the locally-running v3-engine gives us the easiest experience here, as we can get away with just specifying the role in the header, since we're just using the dev auth hook:

```
> ddn connector plugin --connector ./connector.yaml -- generate-supergraph-client --url http://localhost:3000/graphql -H "x-hasura-role: user"
```

But if they want to use a cloud Supergraph build, then they'd need to specify headers with whatever their authentication material is (ie. a bearer JWT), which might be annoying for them to easily acquire by themselves:

```
> ddn connector plugin --connector ./connector.yaml -- generate-supergraph-client --url https://settling-impala-7722-50bb46e1d5.ddn.hasura.app/graphql -H "Authorization: Bearer ey..."
```

### Get v3-engine to output an SDL file
GraphQL client tooling is usually able to take a GraphQL SDL file that describes the schema as input into their code generation tools. We could invoke v3-engine locally and have it generate an SDL file:

```
> ddn supergraph build local --out ./engine --sdl-out ./app/connector/my_fn/schema.graphql --sdl-role user
```

One advantage of this approach is avoiding having to deal with authentication. We can simply ask the engine to output the SDL for the specified role. The engine would not actually have to be running to get the SDL; we would invoke it as a batch process to simply output the SDL (this functionality would need to be built). The SDL file could also be useful for the user to inspect their editor as a local copy of the GraphQL schema they can navigate using their editor's GraphQL extensions (as opposed to schema introspection JSON, which is relatively unreadable).
