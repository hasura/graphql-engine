// custom type definitions
const typeDefs = `
  type Query {
    hello: String,
    count: Int,
    user_average_age: Float
  }

  type Mutation {
    increment_counter: MutationResp
  }

  type MutationResp {
    new_count: Int
  }
`;

export default typeDefs;
