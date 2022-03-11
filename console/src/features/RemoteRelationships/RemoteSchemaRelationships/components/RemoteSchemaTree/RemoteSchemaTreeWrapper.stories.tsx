import React from 'react';
import { Story, Meta } from '@storybook/react';
import { buildClientSchema } from 'graphql';
import countries from './fixtures/countries.json';
import hasura_schema from './fixtures/hasura_schema.json';
import { customer_columns, remote_rel_definition } from './fixtures/constants';

import {
  RemoteSchemaTreeWrapper,
  RemoteSchemaTreeWrapperProps,
} from './RemoteSchemaTreeWrapper';

export default {
  title: 'Remote Relationships/Components/Remote Schema Tree Wrapper',
  component: RemoteSchemaTreeWrapper,
} as Meta;

export const RemoteSchemaTreeWithCountriesSchema: Story<RemoteSchemaTreeWrapperProps> = args => (
  <RemoteSchemaTreeWrapper {...args} />
);
RemoteSchemaTreeWithCountriesSchema.args = {
  schema: buildClientSchema(countries as any),
  columns: customer_columns,
  rootFields: ['query', 'mutation', 'subscription'],
};
RemoteSchemaTreeWithCountriesSchema.parameters = {
  // Disable chromatic snapshot for playground stories
  chromatic: { disableSnapshot: true },
};

export const RemoteSchemaTreeWithHasuraCloudSchema: Story<RemoteSchemaTreeWrapperProps> = args => (
  <RemoteSchemaTreeWrapper {...args} />
);
RemoteSchemaTreeWithHasuraCloudSchema.args = {
  schema: buildClientSchema(hasura_schema as any),
  columns: customer_columns,
  rootFields: ['query', 'mutation'],
};
RemoteSchemaTreeWithHasuraCloudSchema.parameters = {
  // Disable chromatic snapshot for playground stories
  chromatic: { disableSnapshot: true },
};

export const RemoteSchemaTreeWithExistingRelationship: Story<RemoteSchemaTreeWrapperProps> = args => (
  <RemoteSchemaTreeWrapper {...args} />
);
RemoteSchemaTreeWithExistingRelationship.args = {
  schema: buildClientSchema(hasura_schema as any),
  columns: customer_columns,
  rootFields: ['query', 'mutation'],
  serverRelationship: remote_rel_definition as any,
};
