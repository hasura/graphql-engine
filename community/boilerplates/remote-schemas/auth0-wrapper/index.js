const fetch = require('node-fetch');
const { ApolloServer, ApolloError } = require('apollo-server');
const { makeExecutableSchema } = require('graphql-tools');

const ADMIN_TOKEN = process.env.AUTH0_ADMIN_TOKEN;
const ENDPOINT = process.env.AUTH0_ENDPOINT

const OPTIONS = {
  method: 'GET',
  headers: {
    "Authorization": `Bearer ${ADMIN_TOKEN}`
  }
};

const getUserFromAuth0 = async (auth0_id) => {
  if (!ADMIN_TOKEN) {
    throw new ApolloError("no admin token provided");
  }

  const response = await fetch(
    `${ENDPOINT}/${auth0_id}`,
    OPTIONS
  );

  try {
    const respObj = await response.json();
    return respObj;
  } catch (e) {
    throw new ApolloError(e);
  }
}

const getUserFromAuth0Email = async (email) => {
  if (!ADMIN_TOKEN) {
    throw new ApolloError("no admin token provided");
  }

  const response = await fetch(
    ENDPOINT,
    OPTIONS
  );

  try {
    const respObj = await response.json();
    return respObj.find(u => u.email === email);
  } catch (e) {
    throw new ApolloError(e);
  }
}

const typeDefs = `
  type Query {
    auth0 (auth0_id: String, email: String): Auth0Info
  }

  type Auth0Info {
    user_id: String,
    email: String,
    email_verified: Boolean,
    name: String,
    picture: String,
    nickname: String,
    created_at: String,
    last_login: String,
    logins_count: Int
  }
`;

const resolvers = {
  Query: {
    auth0: async (_, args) => {
      let response;
      try {
        if (args.auth0_id) {
          response = await getUserFromAuth0(args.auth0_id);

        } else {
          response = await getUserFromAuth0Email(args.email);
        }
        return response;
      } catch (e) {
        throw e;
      }
    }
  }
}

const schema = makeExecutableSchema({ typeDefs, resolvers });

const server = new ApolloServer({ schema, introspection: true, playground: true });

server.listen(process.env.PORT || 3000).then(({ url }) => {
  console.log('Listening at ' + url);
});
