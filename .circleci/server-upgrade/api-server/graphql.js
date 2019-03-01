import { buildSchema } from 'graphql';

const schema = buildSchema(`
  type Query {
    hello:  String
  }
`);

const root = {
  hello: () => {
    return 'Hello world!';
  }
};

export { schema, root };
