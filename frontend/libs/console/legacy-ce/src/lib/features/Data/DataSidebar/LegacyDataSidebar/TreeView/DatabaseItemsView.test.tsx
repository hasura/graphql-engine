import { render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { MemoryRouter, useLocation } from 'react-router';
import { dataRoutes } from '@hasura/shared/utils';
import DatabaseItemsView from './DatabaseItemsView';
import { SourceItem } from '../types';

const items: SourceItem[] = [
  {
    name: 'public.users',
    schema: 'public',
    tableName: 'users',
    table: { schema: 'public', name: 'users' },
    source: 'default',
    type: 'table',
  },
];

const CurrentLocation = () => {
  const { pathname } = useLocation();
  return <output aria-label="Current location">{pathname}</output>;
};

const setup = (source: string | undefined) =>
  render(
    <MemoryRouter>
      <DatabaseItemsView
        sourceName="default"
        params={{ source, schema: null, tableName: undefined }}
        items={items}
        databaseLoading={false}
        schemaLoading={false}
      />
      <CurrentLocation />
    </MemoryRouter>,
  );

describe('DatabaseItemsView', () => {
  it.each([
    ['active', 'default'],
    ['inactive', 'other'],
  ])('left-aligns the full-width %s database row', (_state, source) => {
    setup(source);

    const button = screen.getByText('default').closest('button');

    expect(button).toHaveClass('w-full!', 'justify-start!', 'text-left!');
  });

  it('shows schemas only for the active database', () => {
    const { unmount } = setup('default');

    expect(screen.getByText('public')).toBeInTheDocument();

    unmount();
    setup('other');

    expect(screen.queryByText('public')).not.toBeInTheDocument();
  });

  it('still navigates when the database row is clicked', async () => {
    setup(undefined);

    await userEvent.click(screen.getByText('default'));

    expect(screen.getByLabelText('Current location')).toHaveTextContent(
      dataRoutes.manageDatabaseSource('default'),
    );
  });
});
