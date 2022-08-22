# Use a Go Backend Server With Hasura

The Hasura GraphQL engine instantly generates a real-time GraphQL CRUD API on your data. For some use-cases, we may need to call a custom backend server. This article uses the programming language [Python](https://www.python.org/) to run custom business logic, respond to event triggers, create a GraphQL remote schema, and query a GraphQL endpoint.

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

Create the action, click the `Codegen` tab, and select `python-fast-api`.

Copy `login.py` to `main.py` and `loginTypes.py` in your project.

```python
from fastapi import FastAPI
from typing import Generic, TypeVar
from pydantic import BaseModel
from pydantic.generics import GenericModel
from loginTypes import LoginResponse, loginArgs


ActionInput = TypeVar("ActionInput", bound=BaseModel | None)


class ActionName(BaseModel):
    name: str


class ActionPayload(GenericModel, Generic[ActionInput]):
    action: ActionName
    input: ActionInput
    request_query: str
    session_variables: dict[str, str]


app = FastAPI()


@app.post("/login")
async def actionHandler(action: ActionPayload[loginArgs]) -> LoginResponse:
    return LoginResponse(AccessToken="<sample value>")

```

Install the dependencies and run the app

```bash
pip install "fastapi[all]"

uvicorn main:app --reload
```

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

Let's send a webhook when a new user is created and print out their name.

1.  In the Hasura Console add a `user` table with a `Text` column `name` and the frequently used `UUID` column id.

1.  In the event trigger tab, on the `user` table, check the insert and via console trigger operations.

1.  The event trigger payload schema can be found [in the docs](https://hasura.io/docs/latest/graphql/core/event-triggers/payload/#json-payload). We make pydantic classes in Python to represent this

    ```python
    from pydantic import BaseModel, Field
    from pydantic.generics import GenericModel
    from typing import Generic, Literal, TypeVar

    New = TypeVar("New", bound=BaseModel | None)
    Old = TypeVar("Old", bound=BaseModel | None)


    class DeliveryInfo(BaseModel):
        current_retry: int
        max_retries: int


    class Data(GenericModel, Generic[New, Old]):
        new: New
        old: Old


    class TraceContext(BaseModel):
        span_id: str
        trace_id: str


    class Event(GenericModel, Generic[New, Old]):
        data: Data[New, Old]
        op: Literal["INSERT", "UPDATE", "DELETE", "MANUAL"]
        session_variables: dict[str, str]
        trace_context: TraceContext


    class Table(BaseModel):
        name: str
        schema_: str = Field("", alias="schema")


    class Trigger(BaseModel):
        name: str


    class Payload(GenericModel, Generic[New, Old]):
        created_at: str
        delivery_info: DeliveryInfo
        event: Event[New, Old]
        id: str
        table: Table
        trigger: Trigger

    ```

1.  Now we make an HTTP handler that handles the event

    ```python
    from event import Payload
    class UserTable(BaseModel):
      id: str
      name: str


    @app.post("/event")
    async def actionHandler(action: Payload[UserTable, None]):
        return
    ```

When you add a user in Hasura your Python server should receive the event.

## Remote Schema

We can make a custom GraphQL in Python using [Strawberry](https://strawberry.rocks/) and connect it to Hasura using a [remote schema](https://hasura.io/docs/latest/graphql/core/remote-schemas/index/).

1. Run the [Strawberry FastAPI quickstart](https://strawberry.rocks/docs/integrations/fastapi)

1. In `remoteSchema/remoteSchema.py` add the Strawberry code

   ```python
   import strawberry
   from strawberry.fastapi import GraphQLRouter


   @strawberry.type
   class Query:
       @strawberry.field
       def hello(self) -> str:
         return "Hello World"


   schema = strawberry.Schema(Query)

   graphql_app = GraphQLRouter(schema)
   ```

1. Add the generated GraphQL handler to `main.py`

   ```python
   from remoteSchema.remoteSchema import graphql_app


   app.include_router(graphql_app, prefix="/graphql")
   ```

1. In the Hasura Console remote schema tab, add your Python server `<Python server URL>/graphql`

1. In the API Explorer tab, try querying the sample todos.

   ```graphql
   {
     hello
   }
   ```

## Query GraphQL

To query Hasura from Python we use the async version of [qlient](https://github.com/qlient-org/python-qlient).

1. Install qlient

   ```bash
   pip install qlient.aiohttp
   ```

1. Query all users in the event trigger handler we created earlier,

   ```python
   @app.post("/event")
   async def actionHandler(action: Payload   [UserTable, None]):
       async with AIOHTTPClient("http://graphql-engine:8080/v1/graphql") as client:
           result: GraphQLResponse = await client.query.user(["id", "name"])
           print(result.request.query)
           print(result.data)
       return
   ```

## Conclusion

Hasura autogenerates most of our API but gives us escape hatches for custom logic. We've gone over four ways you can combine the power of Python and Hasura. Enjoy!

When ready to go to production, check out Hasura Cloud for a fully managed Hasura deployment.

<a target="_blank" rel="noopener" href="https://cloud.hasura.io"><img src="https://camo.githubusercontent.com/a6de317cd7d0ed4e8722684b428f72e3da614fe8/68747470733a2f2f6772617068716c2d656e67696e652d63646e2e6861737572612e696f2f696d672f6465706c6f795f746f5f6861737572612e706e67"></a>
