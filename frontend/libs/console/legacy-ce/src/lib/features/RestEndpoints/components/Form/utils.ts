import {
  allowedQueriesCollection,
  AllowedRestMethods,
  RestEndpoint,
} from '@hasura/shared/types';

export type RestEndpointFormData = {
  name: string;
  comment?: string;
  url: string;
  methods: AllowedRestMethods[];
  request: string;
};

export const forgeFormEndpointObject = (
  state: RestEndpointFormData,
): [RestEndpoint, string] => [
  {
    name: state?.name,
    url: state?.url,
    definition: {
      query: {
        query_name: state?.name,
        collection_name: allowedQueriesCollection,
      },
    },
    methods: state?.methods,
    comment: state.comment,
  },
  state.request,
];
