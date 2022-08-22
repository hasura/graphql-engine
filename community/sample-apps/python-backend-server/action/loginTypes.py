from enum import Enum, auto
from pydantic import BaseModel


class LoginResponse(BaseModel):
    AccessToken: str


class Mutation(BaseModel):
    login: LoginResponse | None


class loginArgs(BaseModel):
    username: str
    password: str
