export const allowedQueriesCollection = 'allowed-queries';

export type AllowedRestMethods = 'GET' | 'POST' | 'PUT' | 'PATCH' | 'DELETE';

export interface RestEndpointDefinition {
  query: {
    query_name: string;
    collection_name: string;
  };
}

export interface RestEndpoint {
  name: string;
  url: string;
  methods: AllowedRestMethods[];
  definition: RestEndpointDefinition;
  comment?: string;
}
