const { ApolloServer, ApolloError } = require('apollo-server');
const gql = require('graphql-tag');
const { print } = require('graphql');

const allMessages = [
    { id: 1, name: "alice", msg: "You win!"},
    { id: 2, name: "bob", msg: "You lose!"},
    { id: 3, name: "alice", msg: "Another alice"},
];

const data = {
    1: { title: 'Harry Porter and the prisoner of Azkaban' },
    2: { name: 'J.K. Rowling' },
    3: { title: "Harry Porter and the philoshoper's stone"}
}

const typeDefs = gql`

  type User {
    user_id: Int
    userMessages(whered: MessageWhereInpObj, includes: IncludeInpObj): [Message]
    gimmeText(text: String): String
  }

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

  union SearchResult = Book | Author

  type Book {
    title: String
  }

  type Author {
    name: String
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

  enum Occupation {
    SINGER
    COMEDIAN
    ARTIST
  }

  type Query {
    hello: String
    messages(where: MessageWhereInpObj, includes: IncludeInpObj): [Message]
    user(user_id: ID!): User
    users(user_ids: [ID]!): [User]
    message(id: Int!) : Message
    communications(id: Int): [Communication]
    search(id: Int!): SearchResult
    getOccupation(name: ID!): Occupation!
    getGrade(marks: Int!): String
  }
`;

const resolvers = {

    User: {
        userMessages: (parent, { whered, includes }) => {
            var result = allMessages.filter(m => m.id == parent.user_id);
            if (whered && whered.id) {
                var intExp = whered.id;
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
            if (whered && whered.name) {
                var stringExp = whered.name;
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

        gimmeText: (_, { text }) => {
            if (text) {
                return text;
            } else {
                return "no text";
            }
        }
    },
    Message: {
        errorMsg : () => {
            throw new ApolloError("intentional-error", "you asked for it");
        }
    },

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
        user: (_, { user_id }) => {
            return { "user_id": user_id };
        },
        users: (parent, args, context, info) => {
            var results = []
            for (userId of args.user_ids) {
                results.push({"user_id":userId})
            }
            return results;
        },
        communications: (_, { id }) => {
            var result = allMessages;
            if(id) {
                result = allMessages.filter(m => m.id == id);
            }
            return result;
        },
        search: (_, { id }) => {
            return data[id]
        },
        getOccupation: (_, { name }) => {
            switch (name) {
            case "alice":
                return 'SINGER'
            case "bob":
                return 'ARTIST'
            default:
                throw new ApolloError("invalid argument - " + name, "invalid ");
            }
        },
        getGrade: (_, { marks }) => {
            if(marks > 90) {
                return 'S'
            }
            else if (marks > 80) {
                return 'A'
            }
            else {
                return 'B'
            }
        }
    },
    Communication: {
        __resolveType(communication, context, info){
            if(communication.name) {
                return "Message";
            }
            return null;
        }
    },
    SearchResult: {
        __resolveType(obj, context, info) {
            if(obj.name) {
                return "Author";
            }

            if(obj.title) {
                return "Book";
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
