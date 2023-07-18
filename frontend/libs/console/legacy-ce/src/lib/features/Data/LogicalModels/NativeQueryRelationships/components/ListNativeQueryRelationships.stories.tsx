import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import {
  ListNativeQueryRelationships,
  ListNativeQueryRow,
} from './ListNativeQueryRelationships';
import { ReduxDecorator } from '../../../../../storybook/decorators/redux-decorator';
import { handlers } from '../mocks/handlers';
import globals from '../../../../../Globals';
import { userEvent, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { useState } from 'react';

export default {
  component: ListNativeQueryRelationships,
  decorators: [
    ReactQueryDecorator(),
    ReduxDecorator({
      tables: {
        dataHeaders: {
          'x-hasura-admin-secret': globals.adminSecret as any,
        },
      },
    }),
  ],
} as Meta<typeof ListNativeQueryRelationships>;

export const Basic: StoryObj<typeof ListNativeQueryRelationships> = {
  render: () => (
    <ListNativeQueryRelationships
      dataSourceName="chinook"
      nativeQueryName="get_authors"
    />
  ),
  parameters: {
    msw: handlers(),
  },
};

export const TestBasicFlow: StoryObj<typeof ListNativeQueryRelationships> = {
  render: () => {
    const [result, updateResult] = useState<ListNativeQueryRow>();

    return (
      <div>
        <ListNativeQueryRelationships
          dataSourceName="chinook"
          nativeQueryName="get_authors"
          onEditRow={data => updateResult(data)}
          onDeleteRow={data => updateResult(data)}
        />
        <div data-testid="result">{JSON.stringify(result)}</div>
      </div>
    );
  },
  parameters: {
    msw: handlers(),
  },
  name: '🧪 Basic render and edit/delete action',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    const nativeQueryRelationshipsTable = await canvas.findByTestId(
      'native-query-relationships'
    );

    await expect(nativeQueryRelationshipsTable).toBeInTheDocument();

    /**
     * Check if both header and body have rendered
     */
    await expect(nativeQueryRelationshipsTable.children.length).toEqual(2);

    const rows = await canvas.findAllByTestId(
      /^native-query-relationships-row-.*$/
    );
    /**
     * There should be two rows
     */
    await expect(rows.length).toEqual(2);
    let rowValues = await canvas.findAllByTestId(
      /^native-query-relationships-cell-0-*.*$/
    );

    /**
     * Verify the row values
     */
    await expect(rowValues[0]).toHaveTextContent('articles');
    await expect(rowValues[1]).toHaveTextContent('array');

    let editButton = await within(rowValues[2]).findByTestId('edit-button');
    await userEvent.click(editButton);

    await expect(await canvas.getByTestId('result')).toHaveTextContent(
      JSON.stringify({
        name: 'articles',
        using: {
          column_mapping: { id: 'author_id' },
          insertion_order: null,
          remote_native_query: 'get_article',
        },
        type: 'array',
      })
    );

    let deleteBtn = await within(rowValues[2]).findByTestId('delete-button');
    await userEvent.click(deleteBtn);
    await expect(await canvas.getByTestId('result')).toHaveTextContent(
      JSON.stringify({
        name: 'articles',
        using: {
          column_mapping: { id: 'author_id' },
          insertion_order: null,
          remote_native_query: 'get_article',
        },
        type: 'array',
      })
    );

    rowValues = await canvas.findAllByTestId(
      /^native-query-relationships-cell-1-*.*$/
    );

    await expect(rowValues[0]).toHaveTextContent('author_details');
    await expect(rowValues[1]).toHaveTextContent('object');

    editButton = await within(rowValues[2]).findByTestId('edit-button');
    await userEvent.click(editButton);

    await expect(await canvas.getByTestId('result')).toHaveTextContent(
      JSON.stringify({
        name: 'author_details',
        using: {
          column_mapping: { id: 'author_id' },
          insertion_order: null,
          remote_native_query: 'get_author_details',
        },
        type: 'object',
      })
    );

    deleteBtn = await within(rowValues[2]).findByTestId('delete-button');
    await userEvent.click(deleteBtn);
    await expect(await canvas.getByTestId('result')).toHaveTextContent(
      JSON.stringify({
        name: 'author_details',
        using: {
          column_mapping: { id: 'author_id' },
          insertion_order: null,
          remote_native_query: 'get_author_details',
        },
        type: 'object',
      })
    );
  },
};
