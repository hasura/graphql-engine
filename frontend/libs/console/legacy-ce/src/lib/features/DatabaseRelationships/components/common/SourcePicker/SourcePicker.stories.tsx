import React from 'react';
import { z } from 'zod';
import { action } from '@storybook/addon-actions';
import { FaArrowAltCircleRight } from 'react-icons/fa';
import { StoryFn, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { SimpleForm } from '../../../../../new-components/Form';
import { SourcePicker } from './SourcePicker';
import { SourceSelectorItem } from './SourcePicker.types';
import { mapItemsToSourceOptions } from './SourcePicker.utils';

export default {
  component: SourcePicker,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof SourcePicker>;

const items: SourceSelectorItem[] = [
  {
    type: 'table',
    value: {
      dataSourceName: 'dataSource',
      table: { name: 'aTable', schema: 'schema' },
    },
  },
  {
    type: 'remoteSchema',
    value: { remoteSchemaName: 'remote schema' },
  },
];

const validationSchema = z.object({
  fromSource: z.any(),
});

export const Basic: StoryFn<typeof SourcePicker> = () => (
  <SimpleForm schema={validationSchema} onSubmit={action('onSubmit')}>
    <SourcePicker name="from" items={items} label="My label" />
  </SimpleForm>
);

const defaultValue = mapItemsToSourceOptions([items[0]])[0];
export const Preselected: StoryFn<typeof SourcePicker> = () => {
  return (
    <SimpleForm
      schema={validationSchema}
      onSubmit={action('onSubmit')}
      options={{
        defaultValues: {
          fromSource: defaultValue.value,
        },
      }}
    >
      <SourcePicker
        name="fromSource"
        items={items}
        label="Label"
        labelIcon={
          <FaArrowAltCircleRight className="fill-emerald-700 ml-1.5" />
        }
        disabled
      />
    </SimpleForm>
  );
};
