from flask import Flask
from flask_graphql import GraphQLView
import graphene
from models import UserModel, User, MinAmountModel, MinAmount, db_session

# create the flask application
app = Flask(__name__)

# create a query root using graphene
class Query(graphene.ObjectType):
    # create a graphql node
    hello = graphene.String(description='A node which says Hello World!')
    # write resolver for this node
    def resolve_hello(self, info):
        return "Hello World!"

# We consider a user schema where a user can be added only if a custom validation passes.
# The custom validation involves fetching a min amount from a table
# and checking if the user balance is greater than the min amount.
# This will be done in a transaction.

class ValidateAndAddUser(graphene.Mutation):
    class Arguments:
        name = graphene.String()
        balance = graphene.Int()

    id = graphene.Int()
    name = graphene.String()
    balance = graphene.Int()
    def mutate(self, info, name, balance):
        try:
            #fetch min amount
            minAmount = db_session.query(MinAmountModel).one()
            # check balance
            if balance >= minAmount.amount:
                #create user if balance is greater
                user = UserModel(name=name, balance=balance)
                db_session.add(user)
                db_session.commit()
                db_session.refresh(user)
                return ValidateAndAddUser(id = user.id, name = user.name, balance=user.balance)
            else:
                raise ValueError('balance too low, required atleast ' + str(minAmount.amount))
        except Exception as exc:
            raise exc

# mutation root
class Mutation(graphene.ObjectType):
    validateAndAddUser = ValidateAndAddUser.Field()

# create a schema object using the query and mutation roots
schema = graphene.Schema(query=Query, mutation=Mutation)

# bind the graphql view to the flask application
app.add_url_rule('/graphql', view_func=GraphQLView.as_view('graphql', schema=schema, graphiql=True))
