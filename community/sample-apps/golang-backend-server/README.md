# Use a Go Backend Server With Hasura

The Hasura GraphQL engine instantly generates a real-time GraphQL CRUD API on your data. For some use-cases, we may need to call a custom backend server. This article uses the programming language [Go](https://go.dev/) to run custom business logic, respond to event triggers, create a GraphQL remote schema, and query a GraphQL endpoint.

Run the example with Docker

```bash
docker compose up -d
```

- [Hasura Actions](#hasura-actions)
- [Event Triggers](#event-triggers)
- [Remote Schema](#remote-schema)
- [Query GraphQL](#query-graphql)
- [Conclusion](#conclusion)

## Hasura Actions

Actions are a way to extend Hasura's schema with custom business logic using custom queries and mutations. Actions can be added to Hasura to handle various use cases such as data validation, data enrichment from external sources, and any other complex business logic.

In the Actions tab on the Hasura Console we will set up a custom login function

```graphql
type Mutation {
  login(username: String!, password: String!): LoginResponse
}
```

New types definition:

```graphql
type LoginResponse {
  AccessToken: String!
}
```

Create the action, click the `Codegen` tab, and select `go-serve-mux`.

Combine the two generated Go files into `main.go` then run `go run main.go`.

In the Hasura API explorer tab you should now be able to test it

```graphql
mutation {
  login(password: "password", username: "username") {
    AccessToken
  }
}
```

Result:

```json
{
  "data": {
    "login": {
      "AccessToken": "<sample value>"
    }
  }
}
```

## Event Triggers

Hasura can be used to create event triggers on tables in the database. Event triggers reliably capture events on specified tables and invoke HTTP webhooks to carry out any custom logic.

Let's send a webhook when a new user is created and print out their name.

1.  In the Hasura Console add a `user` table with a `Text` column `name` and the frequently used `UUID` column id.

1.  In the event trigger tab, on the `user` table, check the insert and via console trigger operations.

1.  The event trigger payload schema can be found [in the docs](https://hasura.io/docs/latest/graphql/core/event-triggers/payload/#json-payload). We make a struct type in Go to represent this

    ```go
    type EventTriggerPayload[Old interface{}, New interface{}] struct {
        Event struct {
        	SessionVariables struct {
        		XHasuraRole string `json:"x-hasura-role"`
        	} `json:"session_variables"`
        	Op   string `json:"op"`
        	Data struct {
        		Old *Old `json:"old"`
        		New *New `json:"new"`
        	} `json:"data"`
        	TraceContext struct {
        		TraceID string `json:"trace_id"`
        		SpanID  string `json:"span_id"`
        	} `json:"trace_context"`
        } `json:"event"`
        CreatedAt    time.Time `json:"created_at"`
        ID           string    `json:"id"`
        DeliveryInfo struct {
        	MaxRetries   int `json:"max_retries"`
        	CurrentRetry int `json:"current_retry"`
        } `json:"delivery_info"`
        Trigger struct {
        	Name string `json:"name"`
        } `json:"trigger"`
        Table struct {
        	Schema string `json:"schema"`
        	Name   string `json:"name"`
        } `json:"table"`
    }
    ```

1.  Now we make an HTTP handler that handles the event

    ```go
    func NewUserHandler(w http.ResponseWriter, r *http.Request) {
        var u EventTriggerPayload[interface{}, struct {
            Id   string
            Name string
        }]
        err := json.NewDecoder(r.Body).Decode(&u)
        if err != nil {
            http.Error(w, err.Error(), http.StatusBadRequest)
            return
        }
        fmt.Println("Hello", u.Event.Data.New.Name)

        w.WriteHeader(200)
    }

    mux.HandleFunc("/event", event.NewUserHandler)
    ```

When you add a user in Hasura your Go server should receive the event.

## Remote Schema

We can make a custom GraphQL in Go using [gqlgen](https://gqlgen.com/) and connect it to Hasura using a [remote schema](https://hasura.io/docs/latest/graphql/core/remote-schemas/index/).

1. Run the [gqlgen quickstart](https://gqlgen.com/#quick-start), skipping the first step.

1. In the `graph/schema.resolvers.go` Todos resolver return a placeholder test value.

   ```go
   func (r *queryResolver) Todos(ctx context.Context) ([]*model.Todo, error) {
       return []*model.Todo{
           {
               ID:   "test",
               Text: "test",
               Done: false,
               User: &model.User{
                   ID:   "",
                   Name: "",
               },
           },
       }, nil
   }
   ```

1. Delete `server.go` and in `main.go` add the generated GraphQL handler

   ```go
   import (
    "log"
    "net/http"

    "github.com/hasura/graphql-engine/community/sample-apps/golang-backend-server/action"
    "github.com/hasura/graphql-engine/community/sample-apps/golang-backend-server/event"

    "github.com/99designs/gqlgen/graphql/handler"
    "github.com/hasura/graphql-engine/community/sample-apps/golang-backend-server/graph"
    "github.com/hasura/graphql-engine/community/sample-apps/golang-backend-server/graph/generated"
   )

   func main() {
       mux := http.NewServeMux()

       srv := handler.NewDefaultServer(generated.NewExecutableSchema(generated.Config{Resolvers: &graph.Resolver{}}))

       mux.HandleFunc("/graphql", srv.ServeHTTP)

       mux.HandleFunc("/action", action.LoginHandler)

       mux.HandleFunc("/event", event.NewUserHandler)

       err := http.ListenAndServe(":3000", mux)
       log.Fatal(err)
   }
   ```

1. In the Hasura Console remote schema tab, add your Go server `<Go server URL>/graphql`

1. In the API Explorer tab, try querying the sample todos.

   ```graphql
   query {
     todos {
       id
       text
       done
     }
   }
   ```

## Query GraphQL

To query Hasura from Go we use Khan Academy's [genqlient](https://github.com/Khan/genqlient) to generate a type-safe GraphQL client.

1. [Download your Hasura schema](https://hasura.io/docs/latest/graphql/core/guides/export-graphql-schema/#introduction)

   ```bash
   npx --yes graphqurl <Hasura URL>/v1/graphql --introspect > schema.graphql
   ```

1. Add your queries to `genqlient.graphql`

   ```graphql
   query GetUsers {
     user {
       id
       name
     }
   }
   ```

1. Create `genqlient.yaml`

   ```yaml
   schema: schema.graphql
   operations:
     - genqlient.graphql
   generated: generated/generated.go
   use_struct_references: true
   bindings:
     DateTime:
       type: time.Time
     uuid:
       type: string
     Int:
       type: int32
   ```

1. Install and run `genqlient`

   ```bash
   go get github.com/Khan/genqlient

   go run github.com/Khan/genqlient
   ```

1. To test if your setup is working, add a user, then query all users in the event trigger handler we created earlier

   ```go
   ctx := context.Background()
   client := graphql.NewClient("<Hasura URL>/v1/graphql", http.DefaultClient)
   resp, _ := generated.GetUsers(ctx, client)
   for _, value := range resp.GetUser() {
     fmt.Printf("%#v", value)
   }
   ```

## Conclusion

Hasura autogenerates most of our API but gives us escape hatches for custom logic. We've gone over four ways you can combine the power of Go and Hasura. Enjoy!

When ready to go to production, check out Hasura Cloud for a fully managed Hasura deployment.

<a target="_blank" rel="noopener" href="https://cloud.hasura.io"><img src="https://camo.githubusercontent.com/a6de317cd7d0ed4e8722684b428f72e3da614fe8/68747470733a2f2f6772617068716c2d656e67696e652d63646e2e6861737572612e696f2f696d672f6465706c6f795f746f5f6861737572612e706e67"></a>
