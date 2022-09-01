import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { SelectItem } from '@/components/Common/SelectInputSplitField/SelectInputSplitField';
import { action } from '@storybook/addon-actions';
import { FormDecorator } from '@/storybook/decorators/react-hook-form';
import { FilterRows, FilterRowsProps } from './FilterRows';
import { OperatorItem } from './FilterRow';

export default {
  title: 'Features/Browse Rows/Components/FilterRows',
  component: FilterRows,
  decorators: [FormDecorator()],
} as ComponentMeta<typeof FilterRows>;

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

const operatorOptions: OperatorItem[] = [
  {
    label: '[_eq] equals',
    value: '$eq',
  },
  {
    label: '[_neq] not equals',
    value: '$neq',
  },
  {
    label: '[_in] in',
    value: '$in',
    defaultValue: '[]',
  },
  {
    label: '[_like] like',
    value: '$like',
    defaultValue: '%%',
  },
];

const fields: FilterRowsProps['fields'] = [
  {
    id: 'id',
    column: 'name',
    operator: '$eq',
    value: 'aa',
  },
];

export const Primary: ComponentStory<typeof FilterRows> = () => (
  <FilterRows
    columns={columnOptions}
    operators={operatorOptions}
    fields={fields}
    onAdd={action('onAdd')}
    onRemove={action('onRemove')}
    showFirstRemove={false}
  />
);
