# -*- coding: utf-8 -*-

import copy
from enum import Enum
import graphene
import http.server
from http import HTTPStatus
import time
import sys
from typing import Callable, NamedTuple, Optional, Union
from urllib.parse import urlparse

import fixtures.tls
from graphql import GraphQLError
from webserver import MkHandlers, RequestHandler, Response


DEFAULT_PORT = 5000


class Context(NamedTuple):
    hge_urls: Optional[list[str]]


def mkJSONResp(graphql_result, extensions={}):
    return Response(HTTPStatus.OK, {**graphql_result.to_dict(), **extensions},
                    {'Content-Type': 'application/json'})


class HelloWorldHandler(RequestHandler):
    def get(self, request):
        return Response(HTTPStatus.OK, 'hello world')

    def post(self, request):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)

class Hello(graphene.ObjectType):
    hello = graphene.String(arg=graphene.String(default_value="world"))

    delayedHello = graphene.String(arg=graphene.String(default_value="world"))

    def resolve_hello(self, info, arg):
        return "Hello " + arg

    def resolve_delayedHello(self, info, arg):
        time.sleep(10)
        return "Hello " + arg

hello_schema = graphene.Schema(query=Hello, subscription=Hello)

class HelloGraphQL(RequestHandler):
    def get(self, request):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)

    def post(self, request):
        if not request.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = hello_schema.execute(request.json['query'])
        return mkJSONResp(res)

class HelloGraphQLEchoRequest(RequestHandler):
    def get(self, request):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)

    def post(self, request):
        if not request.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = hello_schema.execute(request.json['query'])
        respDict = res.to_dict()

        # Return the result as it is, when we send an introspection query
        if respDict.get('data',{}).get('__schema',{}):
            return mkJSONResp(res)
        # Edit the result to contain, the 'request payload' as part of the response.
        # We can then use this to assert the request payload with the expected response.
        else:
            respDict.get('data', {})['hello'] = request.json
            return Response(HTTPStatus.OK, res.to_dict(),
                    {'Content-Type': 'application/json'})

class HelloGraphQLExtensions(RequestHandler):
    def get(self, request):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)

    def post(self, request):
        if not request.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = hello_schema.execute(request.json['query'])
        extensions = {'extensions': {'message': 'an extra field in response object'}}
        return mkJSONResp(res, extensions)

class User(graphene.ObjectType):
    id = graphene.Int()
    username = graphene.String()
    generateError = graphene.String()

    def __init__(self, id, username):
        self.id = id
        self.username = username

    def resolve_id(self, info):
        return self.id

    def resolve_username(self, info):
        return self.username

    def resolve_generateError(self, info):
        return GraphQLError ('Cannot query field "generateError" on type "User".')

    @staticmethod
    def get_by_id(_id):
        xs = list(filter(lambda u: u.id == _id, all_users))
        if not xs:
            return None
        return xs[0]

all_users = [
    User(1, 'jane'),
    User(2, 'john'),
    User(3, 'joe'),
]

class UserDetailsInput(graphene.InputObjectType):
    id = graphene.Int(required=True)
    username = graphene.String(required=True)

class CreateUserInputObject(graphene.Mutation):
    class Arguments:
        user_data = UserDetailsInput(required=True)

    ok = graphene.Boolean()
    user = graphene.Field(lambda: User)

    def mutate(self, info, user_data=None):
        user = User(
            id = user_data.id,
            username = user_data.username
        )
        all_users.append(user)
        return CreateUserInputObject(ok=True, user = user)

class CreateUser(graphene.Mutation):
    class Arguments:
        id = graphene.Int(required=True)
        username = graphene.String(required=True)

    ok = graphene.Boolean()
    user = graphene.Field(lambda: User)

    def mutate(self, info, id, username):
        user = User(id, username)
        all_users.append(user)
        return CreateUser(ok=True, user=user)

class UserQuery(graphene.ObjectType):
    user = graphene.Field( User
                          , id=graphene.Int(required=True)
                          , user_info=graphene.Argument(graphene.List(UserDetailsInput), required=False))
    allUsers = graphene.List(User)

    def resolve_user(self, info, id, user_info=None):
        return User.get_by_id(id)

    def resolve_allUsers(self, info):
        return all_users

class UserMutation(graphene.ObjectType):
    createUser = CreateUser.Field()
    createUserInputObj = CreateUserInputObject.Field()

user_schema = graphene.Schema(query=UserQuery, mutation=UserMutation)

class UserGraphQL(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = user_schema.execute(req.json['query'])
        return mkJSONResp(res)

class timestamptz(graphene.types.Scalar):
    @staticmethod
    def serialize(t):
        return "2018-12-20"
    @staticmethod
    def parse_literal(s):
        return "2018-12-20"
    @staticmethod
    def parse_value(s):
        return "2018-12-20"

class Country(graphene.ObjectType):
    name = graphene.String()

    def __init__(self, name):
        self.name = name

    def resolve_name(self, info):
        return self.name

class CountryQuery(graphene.ObjectType):
    country = graphene.Field(Country)

    def resolve_country(self, info):
        return Country("India")

country_schema = graphene.Schema(query=CountryQuery)

class CountryGraphQL(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = country_schema.execute(req.json['query'])
        return mkJSONResp(res)


class person(graphene.ObjectType):
    id = graphene.Int(required=True)
    name = graphene.String()
    created = graphene.Field(timestamptz)

    def resolve_id(self, info):
        return 42
    def resolve_name(self, info):
        return 'Arthur Dent'
    def resolve_created(self, info):
        return '2018-12-20'

class PersonQuery(graphene.ObjectType):
    person_ = graphene.Field(person)

    def resolve_person_(self, info):
        return person()

person_schema = graphene.Schema(query=PersonQuery)

class PersonGraphQL(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = person_schema.execute(req.json['query'])
        return mkJSONResp(res)

# GraphQL server that returns Set-Cookie response header
class SampleAuth(graphene.ObjectType):
    hello = graphene.String(arg=graphene.String(default_value="world"))

    def resolve_hello(self, info, arg):
        return "Hello " + arg

sample_auth_schema = graphene.Schema(query=SampleAuth,
                                     subscription=SampleAuth)

class SampleAuthGraphQL(RequestHandler):
    def get(self, request):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)

    def post(self, request):
        if not request.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = sample_auth_schema.execute(request.json['query'])
        resp = mkJSONResp(res)
        resp.headers['Set-Cookie'] = 'abcd'
        resp.headers['Custom-Header'] = 'custom-value'
        return resp

# GraphQL server that can return arbitrary size result
class BigInterface(graphene.Interface):
    hello = graphene.Field(graphene.String)

class Big(graphene.ObjectType):
    class Meta:
        interfaces = (BigInterface, )

    big = graphene.Field(BigInterface, required=False)
    many = graphene.Field(graphene.List(BigInterface), required=False, arg=graphene.Int(default_value=10))

    # hello = graphene.Field(graphene.String)

    def resolve_hello(self, info):
        return "Hello"

    def resolve_big(self, info):
        return self

    def resolve_many(self, info, arg):
        for i in range(arg):
            yield self

class BigQuery(graphene.ObjectType):
    # start = graphene.Field(BigInterface)
    start = graphene.Field(Big)

    def resolve_start(self, info):
        return Big()

big_schema = graphene.Schema(query=BigQuery)

class BigGraphQL(RequestHandler):
    def get(self, request):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)

    def post(self, request):
        if not request.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = big_schema.execute(request.json['query'])
        resp = mkJSONResp(res)
        resp.headers['Set-Cookie'] = 'abcd'
        resp.headers['Custom-Header'] = 'custom-value'
        return resp


# GraphQL server with interfaces

class Character(graphene.Interface):
    id = graphene.ID(required=True)
    name = graphene.String(required=True)

    def __init__(self, id, name):
        self.id = id
        self.name = name

class Droid(graphene.ObjectType):
    class Meta:
        interfaces = (Character, )

    primary_function = graphene.String()

    def __init__(self, primary_function, character):
        self.primary_function = primary_function
        self.character = character

    def resolve_id(self, info):
        return self.character.id

    def resolve_name(self, info):
        return self.character.name

    def resolve_primary_function(self, info):
        return self.primary_function

class Human(graphene.ObjectType):
    class Meta:
        interfaces = (Character, )

    home_planet = graphene.String()

    droid = graphene.Field(Droid, required=False)

    def __init__(self, home_planet, droid, character):
        self.home_planet = home_planet
        self.character = character
        self.droid = droid

    def resolve_id(self, info):
        return self.character.id

    def resolve_name(self, info):
        return self.character.name

    def resolve_primary_function(self, info):
        return self.home_planet

    def resolve_droid(self, info):
        return self.droid

class CharacterSearchResult(graphene.Union):
    class Meta:
        types = (Human,Droid)

r2 = Droid("Astromech", Character(1,'R2-D2'))
all_characters = {
 4: r2,
 5: Human("Tatooine", r2, Character(2, "Luke Skywalker")),
}

character_search_results = {
 1: Droid("Astromech", Character(6,'R2-D2')),
 2: Human("Tatooine", r2, Character(7, "Luke Skywalker")),
}

class CharacterInputArgs(graphene.InputObjectType):
    episode = graphene.Int(required=True)

class CharacterIFaceQuery(graphene.ObjectType):
    hero = graphene.Field(
        Character,
        required=False,
        episode=graphene.Int(required=True)
    )

    heroes = graphene.Field(
        graphene.List(Character),
        required=False
    )

    hero_by_args = graphene.Field(
        Character,
        required=False,
        arguments=CharacterInputArgs(required=True)
    )

    def resolve_hero(_, info, episode):
        return all_characters.get(episode)

    def resolve_heroes(_, info):
        return all_characters.values()

    def resolve_hero_by_args(_, info, arguments):
        return all_characters.get(arguments.episode)

schema = graphene.Schema(query=CharacterIFaceQuery, types=[Human, Droid])

character_interface_schema = graphene.Schema(query=CharacterIFaceQuery, types=[Human, Droid])

class CharacterInterfaceGraphQL(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = character_interface_schema.execute(req.json['query'], variable_values=req.json.get('variables'))
        return mkJSONResp(res)

class InterfaceGraphQLErrEmptyFieldList(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = character_interface_schema.execute(req.json['query'])
        respDict = res.to_dict()
        typesList = respDict.get('data',{}).get('__schema',{}).get('types',None)
        if typesList is not None:
            for t in typesList:
                if t['kind'] == 'INTERFACE':
                  t['fields'] = []
        return Response(HTTPStatus.OK, respDict,
                    {'Content-Type': 'application/json'})

class InterfaceGraphQLErrUnknownInterface(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = character_interface_schema.execute(req.json['query'])
        respDict = res.to_dict()
        typesList = respDict.get('data',{}).get('__schema',{}).get('types',None)
        if typesList is not None:
            for t in typesList:
                if t['kind'] == 'OBJECT' and t['name'] == 'Droid':
                    t['interfaces'][0]['name'] = 'UnknownIFace'
        return Response(HTTPStatus.OK, respDict,
                    {'Content-Type': 'application/json'})

class InterfaceGraphQLErrWrongFieldType(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = character_interface_schema.execute(req.json['query'])
        respDict = res.to_dict()
        typesList = respDict.get('data',{}).get('__schema',{}).get('types',None)
        if typesList is not None:
            for t in typesList:
                #Remove id field from Droid
                if t['kind'] == 'OBJECT' and t['name'] == 'Droid':
                    for f in t['fields'].copy():
                        if f['name'] == 'id':
                            f['type']['ofType']['name'] = 'String'
        return Response(HTTPStatus.OK, respDict,
                    {'Content-Type': 'application/json'})

class InterfaceGraphQLErrMissingField(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = character_interface_schema.execute(req.json['query'])
        respDict = res.to_dict()
        typesList = respDict.get('data',{}).get('__schema',{}).get('types',None)
        if typesList is not None:
            for t in typesList:
                #Remove id field from Droid
                if t['kind'] == 'OBJECT' and t['name'] == 'Droid':
                    for f in t['fields'].copy():
                        if f['name'] == 'id':
                            t['fields'].remove(f)
        return Response(HTTPStatus.OK, respDict,
                    {'Content-Type': 'application/json'})
ifaceArg = {
    "name": "ifaceArg",
    "description": None,
    "type": {
        "kind": "NON_NULL",
        "name": None,
        "ofType": {
            "kind": "SCALAR",
            "name": "Int",
            "ofType": None
        }
    },
    "defaultValue": None
}

class InterfaceGraphQLErrMissingArg(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = character_interface_schema.execute(req.json['query'])
        respDict = res.to_dict()
        typesList = respDict.get('data',{}).get('__schema',{}).get('types',None)
        if typesList is not None:
            for t in typesList:
                if t['kind'] == 'INTERFACE':
                    for f in t['fields']:
                        if f['name'] == 'id':
                            f['args'].append(ifaceArg)
        return Response(HTTPStatus.OK, respDict,
                    {'Content-Type': 'application/json'})

class InterfaceGraphQLErrWrongArgType(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = character_interface_schema.execute(req.json['query'])
        respDict = res.to_dict()
        objArg = copy.deepcopy(ifaceArg)
        objArg['type']['ofType']['name'] = 'String'

        typesList = respDict.get('data',{}).get('__schema',{}).get('types',None)
        if typesList is not None:
            for t in filter(lambda ty : ty['kind'] == 'INTERFACE', typesList):
                for f in filter(lambda fld: fld['name'] == 'id', t['fields']):
                    f['args'].append(ifaceArg)

            for t in filter(lambda ty: ty['name'] in ['Droid','Human'], typesList):
                for f in filter(lambda fld: fld['name'] == 'id', t['fields']):
                    f['args'].append(ifaceArg if t['name'] == 'Droid' else objArg)

        return Response(HTTPStatus.OK, respDict,
                    {'Content-Type': 'application/json'})

class InterfaceGraphQLErrExtraNonNullArg(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = character_interface_schema.execute(req.json['query'])
        respDict = res.to_dict()
        typesList = respDict.get('data',{}).get('__schema',{}).get('types',None)
        if typesList is not None:
            for t in typesList:
                if t['kind'] == 'OBJECT' and t['name'] == 'Droid':
                    for f in t['fields']:
                        if f['name'] == 'id':
                            f['args'].append({
                                "name": "extraArg",
                                "description": None,
                                "type": {
                                    "kind": "NON_NULL",
                                    "name": None,
                                    "ofType": {
                                        "kind": "SCALAR",
                                        "name": "Int",
                                        "ofType": None
                                    }
                                },
                                "defaultValue": None
                            })
        return Response(HTTPStatus.OK, respDict,
                    {'Content-Type': 'application/json'})

#GraphQL server involving union type

class UnionQuery(graphene.ObjectType):
    search = graphene.Field(
        CharacterSearchResult,
        required=False,
        episode=graphene.Int(required=True)
    )

    def resolve_search(_, info, episode):
        return character_search_results.get(episode)

union_schema = graphene.Schema(query=UnionQuery, types=[Human, Droid])

class UnionGraphQL(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = union_schema.execute(req.json['query'])
        return mkJSONResp(res)

class UnionGraphQLSchemaErrUnknownTypes(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = union_schema.execute(req.json['query'])
        respDict = res.to_dict()
        typesList = respDict.get('data',{}).get('__schema',{}).get('types',None)
        if typesList is not None:
            for t in typesList:
                if t['kind'] == 'UNION':
                    for i, p in enumerate(t['possibleTypes']):
                       p['name'] = 'Unknown' + str(i)
        return Response(HTTPStatus.OK, respDict,
                    {'Content-Type': 'application/json'})

class UnionGraphQLSchemaErrSubTypeInterface(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = union_schema.execute(req.json['query'])
        respDict = res.to_dict()
        typesList = respDict.get('data',{}).get('__schema',{}).get('types',None)
        if typesList is not None:
            for t in typesList:
                if t['kind'] == 'UNION':
                    for p in t['possibleTypes']:
                       p['name'] = 'Character'
        return Response(HTTPStatus.OK, respDict,
                    {'Content-Type': 'application/json'})

class UnionGraphQLSchemaErrNoMemberTypes(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = union_schema.execute(req.json['query'])
        respDict = res.to_dict()
        typesList = respDict.get('data',{}).get('__schema',{}).get('types',None)
        if typesList is not None:
            for t in typesList:
                if t['kind'] == 'UNION':
                    t['possibleTypes'] = []
        return Response(HTTPStatus.OK, respDict,
                    {'Content-Type': 'application/json'})

class UnionGraphQLSchemaErrWrappedType(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = union_schema.execute(req.json['query'])
        respDict = res.to_dict()
        typesList = respDict.get('data',{}).get('__schema',{}).get('types',None)
        if typesList is not None:
            for t in typesList:
                if t['kind'] == 'UNION':
                    for i, p in enumerate(t['possibleTypes']):
                        t['possibleTypes'][i] = {
                            "kind": "NON_NULL",
                            "name": None,
                            "ofType": p
                        }
        return Response(HTTPStatus.OK, respDict,
                    {'Content-Type': 'application/json'})

#GraphQL server with default values for inputTypes

class InpObjType(graphene.InputObjectType):

    @classmethod
    def default(cls):
        meta = cls._meta
        fields = meta.fields
        default_fields = {name: field.default_value for name, field in fields.items()}
        container = meta.container
        return container(**default_fields)

class SizeObj(graphene.ObjectType):
    width = graphene.Int()
    height = graphene.Float()
    shape = graphene.String()
    hasTag = graphene.Boolean()
class Color(Enum):
    RED = 1
    GREEN = 2
    BLUE = 3

GQColorEnum = graphene.Enum.from_enum(Color)

class SizeInput(InpObjType):
    width = graphene.Int(default_value=100)
    height = graphene.Float(default_value=100.1)
    shape = graphene.String(default_value="cube")
    hasTag = graphene.Boolean(default_value=False)

    def asSizeObj(self):
        return SizeObj(width=self.width, height=self.height, shape=self.shape, hasTag=self.hasTag)


class Echo(graphene.ObjectType):
    intFld = graphene.Int()
    listFld = graphene.List(graphene.String)
    objFld = graphene.Field(SizeObj)
    enumFld = graphene.Field(GQColorEnum)

class EchoQuery(graphene.ObjectType):
    echo = graphene.Field(
                Echo,
                int_input=graphene.Int(default_value=1234),
                list_input=graphene.Argument(graphene.List(graphene.String), default_value=["hi","there"]),
                obj_input=graphene.Argument(SizeInput, default_value=SizeInput.default()),
                enum_input=graphene.Argument(GQColorEnum, default_value=GQColorEnum.RED.name),
                r_int_input=graphene.Int(required=True, default_value=1234),
                r_list_input=graphene.Argument(graphene.List(graphene.String, required=True), default_value=["general","Kenobi"]),
                r_obj_input=graphene.Argument(SizeInput, required=True, default_value=SizeInput.default()),
                r_enum_input=graphene.Argument(GQColorEnum, required=True, default_value=GQColorEnum.RED.name),
            )

    def resolve_echo(self, info, int_input, list_input, obj_input, enum_input):
        #print (int_input, list_input, obj_input)
        return Echo(intFld=int_input, listFld=list_input, objFld=obj_input, enumFld=enum_input)

echo_schema = graphene.Schema(query=EchoQuery)

class EchoGraphQL(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = echo_schema.execute(req.json['query'], variables=req.json.get('variables',None))
        resp_dict = res.to_dict()
        types_list = resp_dict.get('data',{}).get('__schema',{}).get('types', None)
        #Hack around enum default_value serialization issue: https://github.com/graphql-python/graphql-core/issues/166
        if types_list is not None:
            for t in filter(lambda ty: ty['name'] == 'EchoQuery', types_list):
                for f in filter(lambda fld: fld['name'] == 'echo', t['fields']):
                    for a in filter(lambda arg: arg['name'] == 'enumInput', f['args']):
                        a['defaultValue'] = 'RED'
        return Response(HTTPStatus.OK, resp_dict,
                    {'Content-Type': 'application/json'})


def header_test_expected_headers(hge_urls: Optional[list[str]]) -> dict[str, Union[list[str], Callable[[list[str]], bool]]]:
    return {
        'authorization': ['Bearer abcdef'],
        'content-type': ['application/json'],
        'x-hasura-role': ['user'],
        'x-hasura-test': ['abcd'],
        'x-hasura-user-id': ['abcd1234'],
        'x-forwarded-host': lambda headers: all(header in hge_urls for header in headers) if hge_urls else True,
        'x-forwarded-user-agent': lambda headers: all(header.startswith('python-requests/') for header in headers),
    }

def header_test_expected_headers_str(hge_urls: Optional[list[str]]):
    expected_headers = header_test_expected_headers(hge_urls)
    return '\n'.join(f'{name}: {value}' for name, value in {
        'authorization': repr(expected_headers['authorization']),
        'content-type': repr(expected_headers['content-type']),
        'x-hasura-role': repr(expected_headers['x-hasura-role']),
        'x-hasura-test': repr(expected_headers['x-hasura-test']),
        'x-hasura-user-id': repr(expected_headers['x-hasura-user-id']),
        'x-forwarded-host': f'one of {hge_urls!r}' if hge_urls else 'ignored',
        'x-forwarded-user-agent': 'starts with "python-requests"',
    }.items())

class HeaderTest(graphene.ObjectType):
    wassup = graphene.String(arg=graphene.String(default_value='world'))

    def resolve_wassup(self, info, arg):
        hge_urls = info.context.context.hge_urls
        actual_headers = info.context.headers
        for header_name, expected_header_values in header_test_expected_headers(hge_urls).items():
            actual_header_values: list[str] = actual_headers.get_all(header_name)
            if callable(expected_header_values):
                if not expected_header_values(actual_header_values):
                    raise Exception(f'Header {header_name} doesn\'t match.\n\nActual headers:\n{actual_headers}Expected headers:\n{header_test_expected_headers_str(hge_urls)}')
            elif expected_header_values != actual_header_values:
                raise Exception(f'Header {header_name} doesn\'t match: {expected_header_values} != {actual_header_values}\n\nActual headers:\n{actual_headers}Expected headers:\n{header_test_expected_headers_str(hge_urls)}')

        return "Hello " + arg

header_test_schema = graphene.Schema(query=HeaderTest)

class HeaderTestGraphQL(RequestHandler):
    def get(self, request):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)

    def post(self, request):
        if not request.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = header_test_schema.execute(request.json['query'], context=request)
        return mkJSONResp(res)


class Message(graphene.ObjectType):
    id = graphene.Int()
    msg = graphene.String()
    def __init__(self, id, msg):
        self.id = id
        self.msg = msg

    def resolve_id(self, info):
        return self.id

    def resolve_msg(self, info):
        return self.msg

    @staticmethod
    def get_by_id(_id):
        xs = list(filter(lambda u: u.id == _id, all_messages))
        if not xs:
            return None
        return xs[0]

all_messages = [
    Message(1, 'You win!'),
    Message(2, 'You lose!')
]

class MessagesQuery(graphene.ObjectType):
    message = graphene.Field(Message, id=graphene.Int(required=True))
    messages = graphene.List(Message)

    def resolve_message(self, info, id):
        return Message.get_by_id(id)

    def resolve_messages(self, info):
        return all_messages

messages_schema = graphene.Schema(query=MessagesQuery)

class MessagesGraphQL(RequestHandler):
    def get(self, request):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)

    def post(self, request):
        if not request.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = messages_schema.execute(request.json['query'])
        return mkJSONResp(res)

class Json(graphene.types.Scalar):
    serialize = lambda x: x

class JsonQuery(graphene.ObjectType):
    json_test = graphene.Field(Json)

    def resolve_json_test(self, info):
        return {"foo\\":"bar","\"foo\"":"bar"}

json_schema = graphene.Schema(query=JsonQuery)

class JsonScalarGraphQL(RequestHandler):
    def get(self, request):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)

    def post(self, request):
        if not request.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = json_schema.execute(request.json['query'])
        return mkJSONResp(res)

def handlers(context):
    return MkHandlers({
        '/hello': HelloWorldHandler,
        '/hello-graphql': HelloGraphQL,
        '/hello-echo-request-graphql': HelloGraphQLEchoRequest,
        '/hello-graphql-extensions': HelloGraphQLExtensions,
        '/user-graphql': UserGraphQL,
        '/country-graphql': CountryGraphQL,
        '/character-iface-graphql' : CharacterInterfaceGraphQL,
        '/iface-graphql-err-empty-field-list' : InterfaceGraphQLErrEmptyFieldList,
        '/iface-graphql-err-unknown-iface' : InterfaceGraphQLErrUnknownInterface,
        '/iface-graphql-err-missing-field' : InterfaceGraphQLErrMissingField,
        '/iface-graphql-err-wrong-field-type' : InterfaceGraphQLErrWrongFieldType,
        '/iface-graphql-err-missing-arg' : InterfaceGraphQLErrMissingArg,
        '/iface-graphql-err-wrong-arg-type' : InterfaceGraphQLErrWrongArgType,
        '/iface-graphql-err-extra-non-null-arg' : InterfaceGraphQLErrExtraNonNullArg,
        '/union-graphql' : UnionGraphQL,
        '/union-graphql-err-unknown-types' : UnionGraphQLSchemaErrUnknownTypes,
        '/union-graphql-err-subtype-iface' : UnionGraphQLSchemaErrSubTypeInterface,
        '/union-graphql-err-no-member-types' : UnionGraphQLSchemaErrNoMemberTypes,
        '/union-graphql-err-wrapped-type' : UnionGraphQLSchemaErrWrappedType,
        '/default-value-echo-graphql' : EchoGraphQL,
        '/person-graphql': PersonGraphQL,
        '/header-graphql': HeaderTestGraphQL,
        '/messages-graphql' : MessagesGraphQL,
        '/auth-graphql': SampleAuthGraphQL,
        '/json-scalar-graphql': JsonScalarGraphQL,
        '/big': BigGraphQL
    }, context)


def create_server(
    server_address: tuple[str, int],
    tls_ca_configuration: Optional[fixtures.tls.TLSCAConfiguration] = None,
    hge_urls: Optional[list[str]] = None,
):
    context = Context([urlparse(url).netloc for url in hge_urls] if hge_urls else None)
    server = http.server.HTTPServer(server_address, handlers(context))
    if tls_ca_configuration:
        server = tls_ca_configuration.configure(server, fixtures.tls.TLSTrust.SECURE)
    return server

def stop_server(server):
    server.shutdown()
    server.server_close()

if __name__ == '__main__':
    port = DEFAULT_PORT
    if len(sys.argv) >= 2:
        port = int(sys.argv[1])
    server = create_server(server_address=('localhost', port))
    server.serve_forever()
