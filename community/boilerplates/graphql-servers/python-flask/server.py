from flask import Flask
from flask_graphql import GraphQLView
import graphene

# create the flask application
app = Flask(__name__)

# create a query root using graphene
class Query(graphene.ObjectType):
    # create a graphql node
    hello = graphene.String(description='A sample GraphQL node')

    # write resolver for this node
    def resolve_hello(self, info):
        return "World"

# create a schema object using the query root
schema = graphene.Schema(query=Query)

# bind the graphql view to the flask application
app.add_url_rule('/graphql', view_func=GraphQLView.as_view('graphql', schema=schema, graphiql=True))
