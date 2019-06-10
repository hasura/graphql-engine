const { ApolloServer, ApolloError } = require('apollo-server');
const gql = require('graphql-tag');


const allMessages = [
    { id: 1, name: "Alice", msg: "You win!"},
    { id: 2, name: "Bob", msg: "You lose!"},
];

const typeDefs = gql`
  type Message {
    id: Int!
    name: String!
    msg: String!
  }

  input MessageWhereInpObj {
    id: IntCompareObj
    name: StringCompareObj
  }

  input IntCompareObj {
    eq : Int
    gt : Int
    lt : Int
  }

  input StringCompareObj {
    eq : String
  }

  type Query {
    hello:  String
    messages(where: MessageWhereInpObj): [Message]
    message(id: Int!) : Message
  }
`;

const resolvers = {
    Query: {
        hello: () => "world",
        message: (_, { id }) => {
            return allMessages.find(m => m.id == id);
        },
        messages: (_, { where }) => {
            var result = allMessages;
            if (where && where.id) {
                var intExp = where.id;
                Object.keys(intExp).forEach(op => {
                    switch(op) {
                    case "eq":
                        result = result.filter(m => m.id == intExp[op]);
                        break;
                    case "gt":
                        result = result.filter(m => m.id > intExp[op]);
                        break;
                    case "lt":
                        result = result.filter(m => m.id < intExp[op]);
                        break;
                    default:
                        throw new ApolloError("invalid argument", "invalid");
                    }
                });
            }
            if (where && where.name) {
                var stringExp = where.name;
                Object.keys(stringExp).forEach(op => {
                    switch(op) {
                    case "eq":
                        result = result.filter(m => m.name == stringExp[op]);
                        break;
                    default:
                        throw new ApolloError("invalid argument", "invalid");
                    }
                });
            }
            return result;
        }
    },
};

const schema = new ApolloServer({ typeDefs, resolvers });

schema.listen({ port: process.env.PORT || 4000 }).then(({ url }) => {
    console.log(`schema ready at ${url}`);
});
