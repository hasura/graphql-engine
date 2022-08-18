import React from 'react';
import * as z from 'zod';
import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { Form } from '@/new-components/Form';
import {
  handlers,
  customer_columns,
  remote_rel_definition,
} from '../../__mocks__';

import {
  RemoteSchemaWidget,
  RemoteSchemaWidgetProps,
} from './RemoteSchemaWidget';

const defaultValues = {
  relationshipMethod: 'remoteSchema',
  name: '',
  type: 'array',
  remoteSchemaFrom: 'remoteSchema1',
  resultSetFrom: '',
  remoteSchemaTo: '',
  resultSet: '',
};

export default {
  title: 'Features/Remote Relationships/Components/Remote Schema Widget',
  component: RemoteSchemaWidget,
  decorators: [
    ReactQueryDecorator(),
    StoryComponent => (
      <Form schema={z.any()} onSubmit={() => {}} options={{ defaultValues }}>
        {() => <StoryComponent />}
      </Form>
    ),
  ],
  parameters: {
    msw: handlers(),
  },
} as Meta;

export const Primary: Story<RemoteSchemaWidgetProps> = args => (
  <RemoteSchemaWidget {...args} />
);
Primary.args = {
  schemaName: 'remoteSchema1',
  fields: customer_columns,
  rootFields: ['query', 'mutation'],
};

Primary.parameters = {
  // Disable chromatic snapshot for playground stories
  chromatic: { disableSnapshot: true },
};

export const RemoteSchemaWidgetWithExistingRelationship: Story<RemoteSchemaWidgetProps> =
  args => <RemoteSchemaWidget {...args} />;
RemoteSchemaWidgetWithExistingRelationship.args = {
  schemaName: 'remoteSchema1',
  fields: customer_columns,
  rootFields: ['query', 'mutation'],
  serverRelationship: remote_rel_definition as any,
};
