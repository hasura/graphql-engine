import knex from 'knex';
import pg from 'pg';

const { PG_CONNECTION_STRING } = process.env;

// create a knex client to connect to directly connect to postgres
pg.defaults.ssl = true;
const knexClient = knex({
  client: 'pg',
  connection: PG_CONNECTION_STRING
});

let count = 0;

// custom resolvers
const resolvers = {
  // resolvers for queries
  Query: {
    hello: (root, args, context, info) => {
      // return response
      return 'Hello world!';
    },
    count: (root, args, context, info) => {
      // return response
      return count;
    },
    user_average_age: async (root, args, context, info) => {
      // make SQL query using knex client
      const response = await knexClient('user')
        .avg('age');
      // return response
      return response[0].avg;
    }
  },

  // resolvers for mutations
  Mutation: {
    increment_counter: (root, args, context, info) => {
      // return response
      return { new_count: ++count };
    }
  }
};

export default resolvers;
