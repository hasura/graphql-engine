import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { SelectItem } from '@/components/Common/SelectInputSplitField/SelectInputSplitField';
import { action } from '@storybook/addon-actions';
import { FormProvider, useForm } from 'react-hook-form';
import { FormDecorator } from '@/storybook/decorators/react-hook-form';
import { SortRows, SortRowsProps } from './SortRows';
import { SortItem } from './SortRow';

export default {
  title: 'Features/Browse Rows/Components/SortRows',
  component: SortRows,
  decorators: [FormDecorator()],
} as ComponentMeta<typeof SortRows>;

const columnOptions: SelectItem[] = [
  {
    label: '-- column --',
    value: '-- column --',
    disabled: true,
  },
  {
    label: 'id',
    value: 'id',
  },
  {
    label: 'name',
    value: 'name',
  },
];

const sortOptions: SortItem[] = [
  {
    label: '--',
    value: '--',
    disabled: true,
  },
  {
    label: 'Asc',
    value: 'asc',
  },
  {
    label: 'Desc',
    value: 'desc',
  },
];

const fields: SortRowsProps['fields'] = [
  {
    id: 'id',
    column: 'name',
    order: 'asc',
  },
];

export const Primary: ComponentStory<typeof SortRows> = () => {
  const methods = useForm();

  return (
    <FormProvider {...methods}>
      <SortRows
        columns={columnOptions}
        orders={sortOptions}
        fields={fields}
        onAdd={action('onAdd')}
        onRemove={action('onRemove')}
        showFirstRemove={false}
      />
    </FormProvider>
  );
};
