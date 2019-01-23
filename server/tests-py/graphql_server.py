# -*- coding: utf-8 -*-

from http import HTTPStatus

import graphene

from webserver import RequestHandler, WebServer, MkHandlers, Response

def mkJSONResp(graphql_result):
    return Response(HTTPStatus.OK, graphql_result.to_dict(),
                    {'Content-Type': 'application/json'})


class HelloWorldHandler(RequestHandler):
    def get(self, request):
        return Response(HTTPStatus.OK, 'hello world')

    def post(self, request):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)

class Hello(graphene.ObjectType):
    hello = graphene.String(arg=graphene.String(default_value="world"))

    def resolve_hello(self, info, arg):
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

class User(graphene.ObjectType):
    id = graphene.Int()
    username = graphene.String()
    def __init__(self, id, username):
        self.id = id
        self.username = username

    def resolve_id(self, info):
        return self.id

    def resolve_username(self, info):
        return self.username

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
    user = graphene.Field(User, id=graphene.Int(required=True))
    allUsers = graphene.List(User)

    def resolve_user(self, info, id):
        return User.get_by_id(id)

    def resolve_allUsers(self, info):
        return all_users

class UserMutation(graphene.ObjectType):
    createUser = CreateUser.Field()
user_schema = graphene.Schema(query=UserQuery, mutation=UserMutation)

class UserGraphQL(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = user_schema.execute(req.json['query'])
        return mkJSONResp(res)

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

    def resolve_id(self, info):
        return 42
    def resolve_name(self, info):
        return 'Arthur Dent'

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

class Character(graphene.Interface):
    id = graphene.ID(required=True)
    name = graphene.String(required=True)

    def __init__(self, id, name):
        self.id = id
        self.name = name

class Human(graphene.ObjectType):
    class Meta:
        interfaces = (Character, )

    home_planet = graphene.String()

    def __init__(self, home_planet, character):
        self.home_planet = home_planet
        self.character = character

    def resolve_id(self, info):
        return self.character.id

    def resolve_name(self, info):
        return self.character.name

    def refolve_primary_function(self, info):
        return self.home_planet

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

class CharacterSearchResult(graphene.Union):
    class Meta:
        types = (Human,Droid)

all_characters = {
 4: Droid("Astromech", Character(1,'R2-D2')),
 5: Human("Tatooine", Character(2, "Luke Skywalker")),
}

character_search_results = {
 1: Droid("Astromech", Character(6,'R2-D2')),
 2: Human("Tatooine", Character(7, "Luke Skywalker")),
}

class CharacterIFaceQuery(graphene.ObjectType):
    hero = graphene.Field(
        Character,
        required=False,
        episode=graphene.Int(required=True)
    )

    def resolve_hero(_, info, episode):
        return all_characters.get(episode)

schema = graphene.Schema(query=CharacterIFaceQuery, types=[Human, Droid])

character_interface_schema = graphene.Schema(query=CharacterIFaceQuery, types=[Human, Droid])

class CharacterInterfaceGraphQL(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = character_interface_schema.execute(req.json['query'])
        return mkJSONResp(res)

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




handlers = MkHandlers({
    '/hello': HelloWorldHandler,
    '/hello-graphql': HelloGraphQL,
    '/user-graphql': UserGraphQL,
    '/country-graphql': CountryGraphQL,
    '/character-iface-graphql' : CharacterInterfaceGraphQL,
    '/union-graphql' : UnionGraphQL,
    '/union-graphql-err-unknown-types' : UnionGraphQLSchemaErrUnknownTypes,
    '/union-graphql-err-subtype-iface' : UnionGraphQLSchemaErrSubTypeInterface,
    '/union-graphql-err-no-member-types' : UnionGraphQLSchemaErrNoMemberTypes,
    '/union-graphql-err-wrapped-type' : UnionGraphQLSchemaErrWrappedType,
    '/person-graphql': PersonGraphQL
})


def create_server(host='127.0.0.1', port=5000):
    return WebServer((host, port), handlers)

def stop_server(server):
    server.shutdown()
    server.server_close()

if __name__ == '__main__':
    s = create_server()
    s.serve_forever()
