export type CustomSchemaFormVals = {
  jsonSchema?: string;
  graphqlSchema?: string;
  schemaType: 'json' | 'graphql';
  schemaSamplingSize: string;
};
