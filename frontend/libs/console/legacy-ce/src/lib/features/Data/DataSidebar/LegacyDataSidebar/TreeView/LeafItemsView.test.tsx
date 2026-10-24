import { render, screen } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { MemoryRouter } from 'react-router';
import LeafItemsView from './LeafItemsView';
import { SourceItem } from '../types';

const table: SourceItem = {
  name: 'public.users',
  schema: 'public',
  tableName: 'users',
  table: { schema: 'public', name: 'users' },
  source: 'default',
  type: 'table',
};

const fn: SourceItem = {
  ...table,
  name: 'public.search_users',
  tableName: 'search_users',
  table: { schema: 'public', name: 'search_users' },
  type: 'function',
};

const setup = (item: SourceItem) =>
  render(
    <Theme>
      <MemoryRouter>
        <LeafItemsView item={item} isActive={false} />
      </MemoryRouter>
    </Theme>,
  );

describe('LeafItemsView', () => {
  it('shows no warning for a consistent table', () => {
    setup(table);

    expect(screen.getByText('users')).toBeInTheDocument();
    expect(
      screen.queryByTestId('sidebar-inconsistency-warning'),
    ).not.toBeInTheDocument();
  });

  it.each([
    ['table', table],
    ['function', fn],
  ])('warns that an inconsistent %s is inconsistent', (_type, item) => {
    setup({
      ...item,
      inconsistency: 'Inconsistent object: no such table/view exists',
    });

    expect(screen.getByText(item.tableName)).toBeInTheDocument();
    expect(
      screen.getByTestId('sidebar-inconsistency-warning'),
    ).toBeInTheDocument();
  });

  it('links a consistent table to its rows', () => {
    setup(table);

    expect(screen.getByRole('link', { name: 'users' })).toHaveAttribute(
      'href',
      expect.stringContaining('/browse'),
    );
  });

  it.each([
    ['table', table],
    ['function', fn],
  ])('links an inconsistent %s to the metadata status', (_type, item) => {
    setup({ ...item, inconsistency: 'Inconsistent object' });

    expect(screen.getByRole('link', { name: item.tableName })).toHaveAttribute(
      'href',
      '/settings/metadata-status',
    );
  });
});
