declare module '@walmartlabs/json-to-simple-graphql-schema/lib' {
  export function jsonToSchema(options: {
    jsonInput: string;
    baseType: string;
  }): { value: string };
}
declare module 'format-graphql' {
  export function formatSdl(query: string): string;
}
