import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { SelectItem } from '@/components/Common/SelectInputSplitField/SelectInputSplitField';
import { action } from '@storybook/addon-actions';
import { FiltersSection } from './FiltersSection';
import { OperatorItem } from './FilterRow';
import { SortItem } from './SortRow';

export default {
  title: 'Features/Browse Rows/Components/FiltersSection',
  component: FiltersSection,
} as ComponentMeta<typeof FiltersSection>;

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

export const Primary: ComponentStory<typeof FiltersSection> = () => (
  <FiltersSection
    columns={columnOptions}
    operators={operatorOptions}
    orders={sortOptions}
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    onExport={action('onExport') as any}
    onSubmit={action('onSubmit')}
  />
);

// NOTE: Chromatic raises an error when this interaction tests are in place
/* 
Primary.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  expect(await canvas.findAllByDisplayValue('-- column --')).toHaveLength(2);
  expect(await canvas.findByDisplayValue('[_eq] equals')).toBeVisible();
  expect(await canvas.findByPlaceholderText('-- value --')).toBeVisible();

  // select the value "id" in the "column" select
  userEvent.selectOptions(
    (await canvas.findAllByDisplayValue('-- column --'))[0],
    'id'
  );

  // select the value "neq" in the "operator" select
  userEvent.selectOptions(
    await canvas.findByDisplayValue('[_eq] equals'),
    '$neq'
  );

  // click on the value textbox
  userEvent.click(await canvas.findByPlaceholderText('-- value --'));

  // type text in the value textbox
  userEvent.type(await canvas.findByPlaceholderText('-- value --'), 'test');

  // click on the Add button
  userEvent.click(canvas.getAllByText('Add')[0]);

  // select the value "name" in the second "column" select
  userEvent.selectOptions(
    (await canvas.findAllByDisplayValue('-- column --'))[0],
    'name'
  );

  // select the value "like" in the second "operator" select
  userEvent.selectOptions(
    await canvas.findByDisplayValue('[_eq] equals'),
    '[_like] like'
  );

  expect(canvas.getAllByRole('textbox')[1]).toHaveValue('%%');

  // click on the second remove button
  userEvent.click(canvas.getAllByRole('button')[1]);

  expect(canvas.getAllByRole('textbox')).toHaveLength(1);
  // click on the remove button
  userEvent.click(canvas.getAllByRole('button')[0]);

  expect((await canvas.findAllByDisplayValue('-- column --'))[0]).toHaveValue(
    '-- column --'
  );

  expect(await canvas.findByPlaceholderText('-- value --')).toHaveValue('');

  // Sort
  expect(await canvas.findByDisplayValue('--')).toBeVisible();

  // select name column for sort
  userEvent.selectOptions(
    (await canvas.findAllByDisplayValue('-- column --'))[1],
    'name'
  );

  // select desc order
  userEvent.selectOptions(await canvas.findByDisplayValue('--'), 'desc');
  // click on the remove button
  userEvent.click(canvas.getAllByRole('button')[1]);

  expect(await canvas.findAllByDisplayValue('-- column --')).toHaveLength(2);
  expect(await canvas.findByDisplayValue('--')).toHaveValue('--');

  // select desc order
  userEvent.selectOptions(await canvas.findByDisplayValue('--'), 'desc');

  // click on the Add button
  userEvent.click(canvas.getAllByText('Add')[1]);

  // select asc order on the second row
  userEvent.selectOptions(await canvas.findByDisplayValue('--'), 'asc');
}; */
