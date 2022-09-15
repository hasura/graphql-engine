export interface RestEndpointDefinition {
  query: {
    query_name: string;
    collection_name: string;
  };
}

export interface RestEndpoint {
  name: string;
  url: string;
  methods: ('POST' | 'GET' | 'PUT' | 'PATCH' | 'DELETE')[];
  definition: RestEndpointDefinition;
  comment?: string;
}
