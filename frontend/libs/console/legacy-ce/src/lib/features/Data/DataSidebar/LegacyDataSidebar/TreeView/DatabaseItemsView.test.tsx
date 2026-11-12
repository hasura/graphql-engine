import { render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { MemoryRouter, useLocation } from 'react-router';
import { Theme } from '@radix-ui/themes';
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
        params={{
          source,
          schema: null,
          tableName: undefined,
          isFunction: false,
        }}
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

  it('lists objects without a schema under the database, not in an unnamed folder', () => {
    const collection: SourceItem = {
      name: 'movies',
      schema: '',
      tableName: 'movies',
      table: ['movies'],
      source: 'default',
      type: 'table',
    };
    render(
      <Theme>
        <MemoryRouter>
          <DatabaseItemsView
            sourceName="default"
            params={{
              source: 'default',
              schema: null,
              tableName: undefined,
              isFunction: false,
            }}
            items={[...items, collection]}
            databaseLoading={false}
            schemaLoading={false}
          />
        </MemoryRouter>
      </Theme>,
    );

    // the collection is listed without expanding a schema folder
    expect(screen.getByText('movies')).toBeInTheDocument();
    expect(screen.getByText('public')).toBeInTheDocument();
  });

  it('hides objects without a schema when the database is not active', () => {
    render(
      <Theme>
        <MemoryRouter>
          <DatabaseItemsView
            sourceName="default"
            params={{
              source: 'other',
              schema: null,
              tableName: undefined,
              isFunction: false,
            }}
            items={[
              {
                name: 'movies',
                schema: '',
                tableName: 'movies',
                table: ['movies'],
                source: 'default',
                type: 'table',
              },
            ]}
            databaseLoading={false}
            schemaLoading={false}
          />
        </MemoryRouter>
      </Theme>,
    );

    expect(screen.queryByText('movies')).not.toBeInTheDocument();
  });

  it('still navigates when the database row is clicked', async () => {
    setup(undefined);

    await userEvent.click(screen.getByText('default'));

    expect(screen.getByLabelText('Current location')).toHaveTextContent(
      dataRoutes.manageDatabaseSource('default'),
    );
  });
});
