# -*- coding: utf-8 -*-

from http import HTTPStatus

import graphene

from webserver import RequestHandler, WebServer, MkHandlers, Response

class HelloWorldHandler(RequestHandler):
    def get(self, request):
        return Response(HTTPStatus.OK, 'hello world')

    def post(self, request):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)

def mkJSONResp(graphql_result):
    return Response(HTTPStatus.OK, graphql_result.to_dict(),
                    {'Content-Type': 'application/json'})


class Hello(graphene.ObjectType):
    hello = graphene.String(arg=graphene.String(default_value="world"))

    def resolve_hello(self, info, arg):
        return "Hello " + arg

simple_schema = graphene.Schema(query=Hello)

class SimpleGraphQL(RequestHandler):
    def get(self, request):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)

    def post(self, request):
        if not request.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = simple_schema.execute(request.json['query'])
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

class Simple2Query(graphene.ObjectType):
    user = graphene.Field(User, id=graphene.Int(required=True))
    allUsers = graphene.List(User)

    def resolve_user(self, info, id):
        return User.get_by_id(id)

    def resolve_allUsers(self, info):
        return all_users

simple2_schema = graphene.Schema(query=Simple2Query)

class Simple2GraphQL(RequestHandler):
    def get(self, req):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)
    def post(self, req):
        if not req.json:
            return Response(HTTPStatus.BAD_REQUEST)
        res = simple2_schema.execute(req.json['query'])
        return mkJSONResp(res)

handlers = MkHandlers({
    '/hello': HelloWorldHandler,
    '/simple-graphql': SimpleGraphQL,
    '/simple2-graphql': Simple2GraphQL
})


def create_server(host='127.0.0.1', port=5000):
    return WebServer((host, port), handlers)

def stop_server(server):
    server.shutdown()
    server.server_close()

if __name__ == '__main__':
    s = create_server()
    s.serve_forever()
