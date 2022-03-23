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
  resultSetTo: '',
};

export default {
  title: 'Remote Relationships/Components/Remote Schema Widget',
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

export const RemoteSchemaWidgetFrom: Story<RemoteSchemaWidgetProps> = args => (
  <RemoteSchemaWidget {...args} />
);
RemoteSchemaWidgetFrom.args = {
  type: 'from',
  schemaList: ['remoteSchema1', 'remoteSchema2'],
  columns: customer_columns,
  rootFields: ['query', 'mutation'],
};
RemoteSchemaWidgetFrom.parameters = {
  // Disable chromatic snapshot for playground stories
  chromatic: { disableSnapshot: true },
};

export const RemoteSchemaWidgetTo: Story<RemoteSchemaWidgetProps> = args => (
  <RemoteSchemaWidget {...args} />
);
RemoteSchemaWidgetTo.args = {
  type: 'to',
  schemaList: ['remoteSchema1', 'remoteSchema2'],
  columns: customer_columns,
  rootFields: ['query', 'mutation'],
};
RemoteSchemaWidgetTo.parameters = {
  // Disable chromatic snapshot for playground stories
  chromatic: { disableSnapshot: true },
};

export const RemoteSchemaWidgetWithExistingRelationship: Story<RemoteSchemaWidgetProps> = args => (
  <RemoteSchemaWidget {...args} />
);
RemoteSchemaWidgetWithExistingRelationship.args = {
  type: 'to',
  schemaList: ['remoteSchema1', 'remoteSchema2'],
  columns: customer_columns,
  rootFields: ['query', 'mutation'],
  serverRelationship: remote_rel_definition as any,
};
