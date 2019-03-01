const { ApolloServer, gql } = require('apollo-server');
const ApolloServerLambda = require('apollo-server-lambda').ApolloServer;
const Sequelize = require("sequelize");
const {User, MinAmount, sequelize} = require('./models.js');

const typeDefs = gql`
  type Query {
    hello:  String
  }

  type Mutation {
    validateAndAddUser(name: String, balance: Int): User
  }

  type User {
    id:       Int
    name:     String
    balance:  Int
  }
`;


// We consider a user schema where a user can be added only if a custom validation passes.
// The custom validation involves fetching a min amount from a table
// and checking if the user balance is greater than the min amount.
// This will be done in a transaction.

const resolvers = {
    Query: {
        hello: () => "world",
    },
    Mutation: {
        validateAndAddUser: async (_, { name, balance }) => {
            //begin transaction
            return await sequelize.transaction(async (t) => {
                try {
                    //fetch min amount
                    const minAmount = await MinAmount.findOne({}, {transaction: t});
                    //check balance
                    if (balance >= minAmount.amount) {
                        //create user if balance is greater
                        const user = await User.create({
                            name: name,
                            balance: balance
                        });
                        return user;
                    } else {
                        throw new Error("balance too low, required atleast " + minAmount.amount);
                    }
                } catch (e) {
                    console.log(e);
                    throw new Error(e);
                }
            });
        }
    }
};

const server = new ApolloServerLambda({
    typeDefs,
    resolvers,
    context: ({ event, context }) => ({
        headers: event.headers,
        functionName: context.functionName,
        event,
        context,
    }),
});

exports.handler = server.createHandler({
    cors: {
        origin: '*',
        credentials: true,
        allowedHeaders: 'Content-Type, Authorization'
    },
});

// For local development
if( process.env.LAMBDA_LOCAL_DEVELOPMENT == "1") {
    const serverLocal = new ApolloServer({ typeDefs, resolvers });

    serverLocal.listen().then(({ url }) => {
        console.log(`Server ready at ${url}`);
    });
}
