const { deriveMutation } = require('./derive');

const handler = (req, res) => {
  const {
    body: {
      types,
      derive: {
        mutation: {
          mutation_name: mutationName
        }
      },
      introspection_schema: introspectionSchema
    }
  } = request;

  const response = deriveMutation(mutationName, introspectionSchema, types);

}