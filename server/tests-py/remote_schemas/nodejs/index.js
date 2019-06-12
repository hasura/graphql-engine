const { ApolloServer, ApolloError } = require('apollo-server');
const gql = require('graphql-tag');


const allMessages = [
    { id: 1, name: "alice", msg: "You win!"},
    { id: 2, name: "bob", msg: "You lose!"},
    { id: 3, name: "alice", msg: "Another alice"},
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

  input IncludeInpObj {
    id: [Int]
    name: [String]
  }

  type Query {
    hello:  String
    messages(where: MessageWhereInpObj, includes: IncludeInpObj): [Message]
    message(id: Int!) : Message
    communications(id: Int): [Communication]
  }
`;

const resolvers = {
    Query: {
        hello: () => "world",
        message: (_, { id }) => {
            return allMessages.find(m => m.id == id);
        },
        messages: (_, { where, includes }) => {
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

            if (includes && includes.id) {
                var ids = includes.id;
                result = result.filter(m => ids.includes(m.id));
            }

            if (includes && includes.name) {
                var names = includes.name;
                result = result.filter(m => names.includes(m.name));
            }

            return result;
        },
        communications: (_, { id }) => {
            var result = allMessages;
            if(id) {
                result = allMessages.filter(m => m.id == id);
            }
            return result;
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

const schema = new ApolloServer({ typeDefs, resolvers });

schema.listen({ port: process.env.PORT || 4000 }).then(({ url }) => {
    console.log(`schema ready at ${url}`);
});
