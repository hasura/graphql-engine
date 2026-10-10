export type GraphQLType = {
  name: string;
  kind: string;
  ofType: GraphQLType;
};
