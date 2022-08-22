from fastapi import FastAPI
from typing import Generic, TypeVar
from pydantic import BaseModel
from pydantic.generics import GenericModel
from action.loginTypes import LoginResponse, loginArgs
from event.event import Payload
from remoteSchema.remoteSchema import graphql_app
from qlient.aiohttp import AIOHTTPClient, GraphQLResponse

ActionInput = TypeVar("ActionInput", bound=BaseModel | None)


class ActionName(BaseModel):
    name: str


class ActionPayload(GenericModel, Generic[ActionInput]):
    action: ActionName
    input: ActionInput
    request_query: str
    session_variables: dict[str, str]


app = FastAPI()


@app.post("/action")
async def actionHandler(action: ActionPayload[loginArgs]) -> LoginResponse:
    action.input
    return LoginResponse(AccessToken="<sample value>")


class UserTable(BaseModel):
    id: str
    name: str


@app.post("/event")
async def actionHandler(action: Payload[UserTable, None]):
    async with AIOHTTPClient("http://graphql-engine:8080/v1/graphql") as client:
        result: GraphQLResponse = await client.query.user(["id", "name"])
        print(result.request.query)
        print(result.data)
    return


app.include_router(graphql_app, prefix="/graphql")
