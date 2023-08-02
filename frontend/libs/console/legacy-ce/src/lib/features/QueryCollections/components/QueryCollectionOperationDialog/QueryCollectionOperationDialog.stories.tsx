import React from 'react';
import { action } from '@storybook/addon-actions';

import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../../storybook/decorators/redux-decorator';
import { Meta } from '@storybook/react';
import { handlers } from '../../../../mocks/metadata.mock';

import { QueryCollectionOperationDialog } from './QueryCollectionOperationDialog';
import { QueryCollectionOperationAdd } from './QueryCollectionOperationAdd';
import { QueryCollectionOperationEdit } from './QueryCollectionOperationEdit';

export default {
  title: 'Features/Query Collections/Query Collection Operation Dialog',
  component: QueryCollectionOperationDialog,
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
} as Meta<typeof QueryCollectionOperationDialog>;

export const AddOperation = () => {
  return (
    <QueryCollectionOperationAdd
      queryCollectionName="allowed-queries"
      onClose={action('onClose')}
    />
  );
};

export const EditOperation = () => {
  const query =
    'query IntrospectionQuery {\n      __schema {\n        queryType { name }\n        mutationType { name }\n        subscriptionType { name }\n        types {\n          ...FullType\n        }\n        directives {\n          name\n          description\n          locations\n          args {\n            ...InputValue\n          }\n        }\n      }\n    }\n\n    fragment FullType on __Type {\n      kind\n      name\n      description\n      fields(includeDeprecated: true) {\n        name\n        description\n        args {\n          ...InputValue\n        }\n        type {\n          ...TypeRef\n        }\n        isDeprecated\n        deprecationReason\n      }\n      inputFields {\n        ...InputValue\n      }\n      interfaces {\n        ...TypeRef\n      }\n      enumValues(includeDeprecated: true) {\n        name\n        description\n        isDeprecated\n        deprecationReason\n      }\n      possibleTypes {\n        ...TypeRef\n      }\n    }\n\n    fragment InputValue on __InputValue {\n      name\n      description\n      type { ...TypeRef }\n      defaultValue\n    }\n\n    fragment TypeRef on __Type {\n      kind\n      name\n      ofType {\n        kind\n        name\n        ofType {\n          kind\n          name\n          ofType {\n            kind\n            name\n            ofType {\n              kind\n              name\n              ofType {\n                kind\n                name\n                ofType {\n                  kind\n                  name\n                  ofType {\n                    kind\n                    name\n                  }\n                }\n              }\n            }\n          }\n        }\n      }\n    }';
  return (
    <QueryCollectionOperationEdit
      queryCollectionName="allowed-queries"
      operation={{
        name: 'introspection query',
        query,
      }}
      onClose={action('onClose')}
    />
  );
};
