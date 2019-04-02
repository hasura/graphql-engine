const { ApolloServer } = require('apollo-server');
const gql = require('graphql-tag');
const {getData, postData} = require('./helpers');

const typeDefs = gql`
  type user {
    id:       String!
    name:     String!
    balance:  Int!
  }

  type Query {
    getUser(id: String!): user
    getAllUsers(name: String): [user]
  }

  type Mutation {
    addUser(name: String!, balance: Int!): user
  }
`;

// replace with actual REST endpoint
const restAPIEndpoint = 'https://fast-lake-87402.herokuapp.com';

const resolvers = {
    Query: {
        getUser: async (_, { id }) => {
            return await getData(restAPIEndpoint + '/users/' + id);
        },

        getAllUsers: async (_, { name }) => {
            var nameParams = '';
            if (name) {
                nameParams = '?name=' + name;
            }
            return await getData(restAPIEndpoint + '/users' + nameParams );
        }
    },

    Mutation: {
        addUser: async (_, { name, balance } ) => {
            return await postData(restAPIEndpoint + '/users', { name, balance } );
        }
    }
};

const schema = new ApolloServer({ typeDefs, resolvers });

schema.listen({ port: process.env.PORT || 4000 }).then(({ url }) => {
    console.log(`schema ready at ${url}`);
});
