const { ApolloServer } = require('apollo-server');
const { makeExecutableSchema } = require('graphql-tools');
const Sequelize = require("sequelize");
const {User, MinAmount, sequelize} = require('./models.js');

const port = process.env.PORT || 4000;

const typeDefs = `
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

const schema = makeExecutableSchema({
  typeDefs,
  resolvers
});

const server = new ApolloServer({
  schema
});

server.listen({ port }).then(({url}) => {
  console.log(`GraphQL server running at ${url}`);
});
