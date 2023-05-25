import React from 'react';
import * as z from 'zod';
import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { action } from '@storybook/addon-actions';
import { SimpleForm } from '../../../../../new-components/Form';
import {
  customer_columns,
  handlers,
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
      <SimpleForm
        schema={z.any()}
        onSubmit={action('onSubmit')}
        options={{ defaultValues }}
        className="p-4"
      >
        <StoryComponent />
      </SimpleForm>
    ),
  ],
  parameters: {
    msw: handlers(),
  },
} as Meta;

export const Primary: StoryObj<RemoteSchemaWidgetProps> = {
  args: {
    schemaName: 'remoteSchema1',
    fields: customer_columns,
    rootFields: ['query', 'mutation'],
  },

  parameters: {
    // Disable chromatic snapshot for playground stories
    chromatic: { disableSnapshot: true },
  },
};

export const RemoteSchemaWidgetWithExistingRelationship: StoryObj<RemoteSchemaWidgetProps> =
  {
    args: {
      schemaName: 'remoteSchema1',
      fields: customer_columns,
      rootFields: ['query', 'mutation'],
      serverRelationship: remote_rel_definition as any,
    },
  };
