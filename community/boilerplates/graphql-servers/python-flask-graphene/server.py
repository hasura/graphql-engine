from flask import Flask
from flask_graphql import GraphQLView
import graphene

# create the flask application
app = Flask(__name__)

# a global variale to store count temporarily
count = 0

# create a query root using graphene
class Query(graphene.ObjectType):
    # create a graphql node
    hello = graphene.String(description='A node which says Hello World!')
    # write resolver for this node
    def resolve_hello(self, info):
        return "Hello World!"

    count = graphene.Int(description='Current value of the counter')
    def resolve_count(self, info):
        return count


# increment_counter mutation
class IncrementCounter(graphene.Mutation):
    new_count = graphene.Int(description='Updated value of the coutner')

    def mutate(self, info):
        global count
        count+=1
        return IncrementCounter(new_count=count)            

# mutation root
class Mutation(graphene.ObjectType):
    increment_counter = IncrementCounter.Field(description='Increment the value of counter by 1')

# create a schema object using the query and mutation roots
schema = graphene.Schema(query=Query, mutation=Mutation)

# bind the graphql view to the flask application
app.add_url_rule('/graphql', view_func=GraphQLView.as_view('graphql', schema=schema, graphiql=True))