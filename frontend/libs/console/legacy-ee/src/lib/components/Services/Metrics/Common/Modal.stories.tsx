import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { action } from '@storybook/addon-actions';

import Modal from './Modal';

const DATA = {
  operation: {
    time: '2023-06-07T12:11:01.134+00:00',
    request_id: 'f09cde5e9616177f77d61c78479ed633',
    operation_id: '7116865cef017c3b09e5c9271b0e182a6dcf4c01',
    operation_name: 'IntrospectionQuery',
    client_name: null,
    user_role: 'admin',
    execution_time: 0.004819542,
    request_size: 1728,
    response_size: 1152,
    error: null,
    query: {
      query:
        '\n    query IntrospectionQuery {\n      __schema {\n        queryType { name }\n        mutationType { name }\n        subscriptionType { name }\n        types {\n          ...FullType\n        }\n        directives {\n          name\n          description\n          locations\n          args {\n            ...InputValue\n          }\n        }\n      }\n    }\n\n    fragment FullType on __Type {\n      kind\n      name\n      description\n      fields(includeDeprecated: true) {\n        name\n        description\n        args {\n          ...InputValue\n        }\n        type {\n          ...TypeRef\n        }\n        isDeprecated\n        deprecationReason\n      }\n      inputFields {\n        ...InputValue\n      }\n      interfaces {\n        ...TypeRef\n      }\n      enumValues(includeDeprecated: true) {\n        name\n        description\n        isDeprecated\n        deprecationReason\n      }\n      possibleTypes {\n        ...TypeRef\n      }\n    }\n\n    fragment InputValue on __InputValue {\n      name\n      description\n      type { ...TypeRef }\n      defaultValue\n    }\n\n    fragment TypeRef on __Type {\n      kind\n      name\n      ofType {\n        kind\n        name\n        ofType {\n          kind\n          name\n          ofType {\n            kind\n            name\n            ofType {\n              kind\n              name\n              ofType {\n                kind\n                name\n                ofType {\n                  kind\n                  name\n                  ofType {\n                    kind\n                    name\n                  }\n                }\n              }\n            }\n          }\n        }\n      }\n    }\n  ',
      variables: {
        review: 4,
      },
    },
    user_vars: {
      'x-hasura-role': 'admin',
    },
    transport: 'ws',

    request_headers: {
      Accept: '*/*',
      'Accept-Encoding': 'gzip, deflate',
      'Accept-Language': 'en',
      'Cache-Control': 'no-cache',
      Connection: 'close',
      'Content-Length': '1728',
      'Content-Type': 'application/json',
      Host: 'tenant1.nginx.hasura.me',
      Origin: 'http://cloud.lux-dev.hasura.me',
      Pragma: 'no-cache',
      Referer: 'http://cloud.lux-dev.hasura.me/',
      'User-Agent':
        'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36',
      'X-Forwarded-Host': 'tenant1.nginx.hasura.me',
      'X-Forwarded-Port': '80',
      'X-Forwarded-Proto': 'http',
      'X-Forwarded-Server': 'ccced495ab5e',
      'X-NginX-Proxy': 'true',
      'X-Request-Id': 'f09cde5e9616177f77d61c78479ed633',
    },
    websocket_id: 'c0cjsdf09cde5e9616177f77d61c78479ed3',
    ws_operation_id: 'c1984fgb9cde5616177f77d61c78479ed3',
    kind: null,
    request_mode: 'single',
    generated_sql: 'SELECT * FROM public.author',
    trace: [
      {
        id: '429793da-9c7d-450e-9a9d-d50283b446ca',
        name: '/v1/graphql',
        parent_id: null,
        span_id: '7903618144220886803',
        time: '2023-06-07T15:14:02.774+00:00',
        duration: 3398250,
        start: '2023-06-07T15:14:02.878461+00:00',
        meta: {
          request_id: '05e83aff90e604b68cd6daa311105f78',
        },
      },
      {
        id: 'a1c38f1d-bd74-4660-a849-0a1e4924ebe9',
        name: 'Query',
        parent_id: '7903618144220886803',
        span_id: '2425353977187282560',
        time: '2023-06-07T15:14:02.774+00:00',
        duration: 49791,
        start: '2023-06-07T15:14:02.881081+00:00',
        meta: {},
      },
    ],
  },
};

export default {
  component: Modal,
} as Meta<typeof Modal>;

export const Basic: StoryObj<typeof Modal> = {
  args: {
    data: DATA.operation,
    configData: {},
    onHide: action('onHide'),
  },
};
