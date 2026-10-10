import React from 'react';
import * as z from 'zod';
import { action } from 'storybook/actions';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { SimpleForm, Button } from '@hasura/shared/ui';

import {
  RsSourceTypeSelector,
  RsSourceTypeSelectorProps,
} from './RsSourceTypeSelector';
import { refRemoteSchemaSelectorKey } from '../RefRsSelector';

const defaultValues = {
  [refRemoteSchemaSelectorKey]: 'remoteSchema2',
};

export default {
  title:
    'Features/Remote Relationships/Components/Remote Schema Source Type Selector',
  component: RsSourceTypeSelector,
  decorators: [
    (StoryComponent) => (
      <SimpleForm
        schema={z.any()}
        onSubmit={action('onSubmit')}
        options={{ defaultValues }}
        className="p-4"
      >
        <div>
          <StoryComponent />
          <Button type="submit">Submit</Button>
        </div>
      </SimpleForm>
    ),
  ],
} as Meta;

export const Primary: StoryObj<RsSourceTypeSelectorProps> = {
  args: {
    types: ['country', 'continent', 'language', 'state'],
    sourceTypeKey: 'type_name',
    nameTypeKey: 'name',
  },

  parameters: {
    // Disable chromatic snapshot for playground stories
    chromatic: { disableSnapshot: true },
  },
};
