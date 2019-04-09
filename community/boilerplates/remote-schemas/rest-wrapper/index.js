const { ApolloServer } = require('apollo-server');
const gql = require('graphql-tag');
const {getData, postData} = require('./helpers');

const typeDefs = gql`
  type RemoteUser {
    userId:   String!
    name:     String!
    balance:  Int!
  }

  type Query {
    getUser(userId: String!, isRand: Boolean): RemoteUser
    getAllUsers(name: String): [RemoteUser]
  }

  type Mutation {
    addUser(name: String!, balance: Int!): RemoteUser
  }
`;

// replace with actual REST endpoint
const restAPIEndpoint = 'https://fast-lake-87402.herokuapp.com';

const resolvers = {
    Query: {
        getUser: async (_, { userId, isRand }) => {
            var res = await getData(restAPIEndpoint + '/users/' + userId);
            res.userId = res.id;
		console.log(res);
            return res;
        },

        getAllUsers: async (_, { name }) => {
            var nameParams = '';
            if (name) {
                nameParams = '?name=' + name;
            }
            var res = await getData(restAPIEndpoint + '/users' + nameParams );
            return res.map((obj) => { obj.userId = obj.id; return obj} );
        }
    },

    Mutation: {
        addUser: async (_, { name, balance } ) => {
            var res =  await postData(restAPIEndpoint + '/users', { name, balance } );
            res.userId = res.id;
		console.log(res);
            return res;
        }
    }
};

const schema = new ApolloServer({ typeDefs, resolvers });

schema.listen({ port: process.env.PORT || 4000 }).then(({ url }) => {
    console.log(`schema ready at ${url}`);
});
