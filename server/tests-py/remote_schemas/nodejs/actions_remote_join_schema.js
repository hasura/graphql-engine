const { ApolloServer, ApolloError } = require('apollo-server');
const gql = require('graphql-tag');
const { print } = require('graphql');


const allMessages = [
    { id: 1, name: "Clarke", msg: "Welcome to the team, Clarke"},
    { id: 2, name: "Alice", msg: "Welcome to the team, Alice"},
];

const typeDefs = gql`

  interface Communication {
    id: Int!
    msg: String!
  }

  type Message implements Communication {
    id: Int!
    name: String!
    msg: String!
    errorMsg: String
  }

  type Query {
    hello: String
    message(id: Int!) : Message
  }
`;

const resolvers = {

    Message: {
        errorMsg : () => {
            throw new ApolloError("intentional-error", "you asked for it");
        }
    },

    Query: {
        hello: () => "world",
        message: (_, { id }) => {
            return allMessages.find(m => m.id == id);
        }
    },
    Communication: {
        __resolveType(communication, context, info){
            if(communication.name) {
                return "Message";
            }
            return null;
        }
    }
};

class BasicLogging {
    requestDidStart({queryString, parsedQuery, variables}) {
        const query = queryString || print(parsedQuery);
        console.log(query);
        console.log(variables);
    }

    willSendResponse({graphqlResponse}) {
        console.log(JSON.stringify(graphqlResponse, null, 2));
    }
}

const schema = new ApolloServer(
    { typeDefs,
      resolvers,
      extensions: [() => new BasicLogging()],
      formatError: (err) => {
          // Stack traces make expected test output brittle and noisey:
          delete err.extensions;
          return err;
      } });

schema.listen({ port: process.env.PORT || 4001 }).then(({ url }) => {
    console.log(`schema ready at ${url}`);
});
