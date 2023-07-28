import React, { useState } from 'react';
import { OrderBy, WhereClause } from '../../../../DataSource';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { userEvent, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';
import { StoryObj, StoryFn, Meta } from '@storybook/react';
import { DataTableOptions } from './DataTableOptions';
import { DEFAULT_PAGE_INDEX, DEFAULT_PAGE_SIZE } from '../constants';
import { handlers } from '../../../__mocks__/handlers.mock';

export default {
  component: DataTableOptions,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers('http://localhost:8080'),
  },
} as Meta<typeof DataTableOptions>;

const ComponentWrapper = () => {
  const [pageIndex, setPageIndex] = useState(DEFAULT_PAGE_INDEX);
  const [pageSize, setPageSize] = useState(DEFAULT_PAGE_SIZE);
  const [whereClauses, setWhereClauses] = React.useState<WhereClause[]>([
    { AlbumId: { _gt: 12 } },
    { ArtistId: { _lt: 12 } },
  ]);
  const [orderByClauses, setOrderClauses] = React.useState<OrderBy[]>([
    { column: 'First Name', type: 'asc' },
    { column: 'Last Name', type: 'desc' },
  ]);

  const [status, updateStatus] = useState('');

  return (
    <>
      <DataTableOptions
        pagination={{
          goToNextPage: () => {
            setPageIndex(currentPage => {
              updateStatus(`pageIndex is : ${currentPage + 1}`);
              return currentPage + 1;
            });
          },
          goToPreviousPage: () => {
            setPageIndex(currentPage => {
              updateStatus(`pageIndex is : ${currentPage - 1}`);
              return currentPage - 1;
            });
          },
          isNextPageDisabled: false,
          isPreviousPageDisabled: pageIndex <= 0,
          pageSize,
          setPageSize: newPageSize => {
            updateStatus(`pageSize is : ${newPageSize}`);
            setPageSize(newPageSize);
          },
        }}
        query={{
          onQuerySearch: () => updateStatus('@runQueryBtn is clicked'),
          onRefreshQueryOptions: () => updateStatus('@resetBtn is clicked'),
          orderByClauses,
          whereClauses,
          supportedOperators: [
            { name: 'equals', value: '_eq' },
            { name: 'not equals', value: '_neq' },
            { name: '>', value: '_gt' },
            { name: '<', value: '_lt' },
            { name: '>=', value: '_gte' },
            { name: '<=', value: '_lte' },
          ],
          removeWhereClause: id => {
            setWhereClauses(whereClauses.filter((_, i) => i !== id));
          },
          removeOrderByClause: id => {
            setOrderClauses(orderByClauses.filter((_, i) => i !== id));
          },
          onExportRows: exportFileFormat => {
            updateStatus(`export to ${exportFileFormat}`);
            return Promise.resolve(new Error());
          },
          onExportSelectedRows: exportFileFormat => {
            updateStatus(`export to ${exportFileFormat}`);
            return Promise.resolve(new Error());
          },
          disableExportSelectedRows: false,
        }}
      />
      <div data-testid="status">{status}</div>
    </>
  );
};

export const Basic: StoryFn<typeof DataTableOptions> = () => (
  <ComponentWrapper />
);

export const Testing: StoryObj<typeof DataTableOptions> = {
  render: () => <ComponentWrapper />,

  name: '🧪 Testing - user interactions',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // the two where clauses should be present
    expect(await canvas.findByText('AlbumId > "12"')).toBeVisible();
    expect(await canvas.findByText('ArtistId < "12"')).toBeVisible();

    // the two order by clauses should be present
    expect(await canvas.findByText('First Name (asc)')).toBeVisible();
    expect(await canvas.findByText('Last Name (desc)')).toBeVisible();

    // The query button should be updated with the number
    expect(await canvas.findByText('Query (4)')).toBeVisible();

    userEvent.click(await canvas.findByTestId('@runQueryBtn'));

    expect(await canvas.findByTestId('status')).toHaveTextContent(
      '@runQueryBtn is clicked'
    );

    userEvent.click(await canvas.findByTestId('@resetBtn'));

    expect(await canvas.findByTestId('status')).toHaveTextContent(
      '@resetBtn is clicked'
    );

    userEvent.click(await canvas.findByTestId('@nextPageBtn'));

    expect(await canvas.findByTestId('status')).toHaveTextContent(
      'pageIndex is : 1'
    );

    userEvent.click(await canvas.findByTestId('@prevPageBtn'));

    expect(await canvas.findByTestId('status')).toHaveTextContent(
      'pageIndex is : 0'
    );

    userEvent.selectOptions(
      await canvas.findByTestId('@rowSizeSelectInput'),
      '30'
    );

    expect(await canvas.findByTestId('status')).toHaveTextContent(
      'pageSize is : 30'
    );
  },
};
