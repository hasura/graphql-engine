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
