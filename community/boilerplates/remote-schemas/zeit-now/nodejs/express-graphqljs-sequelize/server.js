const express = require('express');
const graphqlHTTP = require('express-graphql');
const { buildSchema } = require('graphql');
const Sequelize = require("sequelize");
const {User, MinAmount, sequelize} = require('./models.js');

const port = process.env.port || 4000;

// Construct a schema, using GraphQL schema language
const schema = buildSchema(`
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
`);

// We consider a user schema where a user can be added only if a custom validation passes.
// The custom validation involves fetching a min amount from a table
// and checking if the user balance is greater than the min amount.
// This will be done in a transaction.

const root = {
  hello: () => {
    return 'Hello world!';
  },
  validateAndAddUser: async ({ name, balance }) => {
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
};

var app = express();
app.use('/graphql', graphqlHTTP({
  schema: schema,
  rootValue: root,
  graphiql: true,
}));
app.listen(port);
console.log(`Running a GraphQL API server at localhost:${port}/graphql`);
