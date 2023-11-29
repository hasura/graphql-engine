import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { userEvent, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { LegacyRunQuery } from './LegacyRunQuery';

export default {
  title: 'GDC Console/Browse Rows/parts/Run Query 📁/Legacy 🦠',
  component: LegacyRunQuery,
} as Meta<typeof LegacyRunQuery>;

export const Primary: StoryObj<typeof LegacyRunQuery> = {
  render: () => (
    <LegacyRunQuery
      columns={[
        {
          name: 'ID',
          dataType: 'number',
          consoleDataType: 'number',
          graphQLProperties: {
            name: 'ID',
            scalarType: 'Int',
          },
        },
        {
          name: 'FirstName',
          dataType: 'string',
          consoleDataType: 'string',
          graphQLProperties: {
            name: 'FirstName',
            scalarType: 'String',
          },
        },
        {
          name: 'UpdatedAt',
          dataType: 'string',
          consoleDataType: 'string',
          graphQLProperties: {
            name: 'UpdatedAtCustomName',
            scalarType: 'String',
          },
        },
      ]}
      operators={[
        {
          name: '_eq',
          value: '_eq',
        },
        {
          name: '_neq',
          value: '_neq',
        },
        {
          name: '_gte',
          value: '_gte',
        },
        {
          name: '_lte',
          value: '_lte',
        },
      ]}
      onExport={action('onExport') as any}
      onSubmit={action('onSubmit')}
    />
  ),

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // click on the filter add button
    userEvent.click((await canvas.findAllByText('Add'))[0]);

    // click on the sort add button
    userEvent.click((await canvas.findAllByText('Add'))[1]);

    expect(await canvas.findAllByDisplayValue('Select a column')).toHaveLength(
      2
    );
    expect(await canvas.findByDisplayValue('Select an operator')).toBeVisible();
    expect(await canvas.findByPlaceholderText('-- value --')).toBeVisible();

    // select the value "id" in the "column" select
    userEvent.selectOptions(
      (await canvas.findAllByDisplayValue('Select a column'))[0],
      'ID'
    );

    // select the value "_eq" in the "operator" select
    userEvent.selectOptions(
      await canvas.findByDisplayValue('Select an operator'),
      '_eq'
    );

    // click on the value textbox
    userEvent.click(await canvas.findByPlaceholderText('-- value --'));

    // type text in the value textbox
    userEvent.type(await canvas.findByPlaceholderText('-- value --'), 'test');

    // click on the Add button
    userEvent.click(canvas.getAllByText('Add')[0]);

    // select the value "name" in the second "column" select
    userEvent.selectOptions(
      (await canvas.findAllByDisplayValue('Select a column'))[0],
      'FirstName'
    );

    // select the value "_neq" in the second "operator" select
    userEvent.selectOptions(
      await canvas.findByDisplayValue('Select an operator'),
      '_neq'
    );

    // click on the second remove button
    userEvent.click(canvas.getAllByRole('button')[1]);

    expect(canvas.getAllByRole('textbox')).toHaveLength(1);
    // click on the remove button
    userEvent.click(canvas.getAllByRole('button')[0]);

    // Sort
    // select name column for sort
    userEvent.selectOptions(
      (await canvas.findAllByDisplayValue('Select a column'))[0],
      'FirstName'
    );

    // select desc order
    userEvent.selectOptions(
      await canvas.findByDisplayValue('Order by'),
      'desc'
    );
  },
};
