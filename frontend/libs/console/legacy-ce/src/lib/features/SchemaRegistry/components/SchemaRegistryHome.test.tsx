import * as React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { Theme } from '@radix-ui/themes';
import { SchemaRegistryHome, SchemaChangeList } from './SchemaRegistryHome';
import { EMPTY_UUID_STRING } from '../constants';

// --- mocks -----------------------------------------------------------------
const h = vi.hoisted(() => ({
  navigate: vi.fn(),
  listResponse: { current: { kind: 'loading' } as any },
  urlResponse: { current: { kind: 'loading' } as any },
}));

vi.mock('react-router', async (importOriginal) => {
  const actual = await importOriginal<typeof import('react-router')>();
  return { ...actual, useNavigate: () => h.navigate };
});

vi.mock('../hooks/useGetSchemaRegistryList', () => ({
  useGetSchemaRegistryList: () => h.listResponse.current,
}));

vi.mock('../hooks/useGetURLSchema', () => ({
  useGetURLSchema: () => h.urlResponse.current,
}));

vi.mock('./SchemaChangeDetails', () => ({
  SchemaChangeDetails: ({ schemaId }: { schemaId: string }) => (
    <div data-testid="schema-change-details">{schemaId}</div>
  ),
}));

const dump = {
  schema_hash: 'h1',
  change_recorded_at: '2023-01-01T00:00:00Z',
  id: 'dump-1',
  entry_hash: 'eh1',
  sibling_schemas: [
    { id: 'admin-role-id', hasura_schema_role: 'admin' },
    { id: 'user-role-id', hasura_schema_role: 'user' },
  ],
  schema_tags: [{ id: 'tag-1', name: 'v1', color: '#36A7E6' }],
};

const makeWrapper = () => {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  });
  return ({ children }: { children?: React.ReactNode }) => (
    <QueryClientProvider client={queryClient}>
      <Theme>{children}</Theme>
    </QueryClientProvider>
  );
};

const renderHome = (schemaId: string | undefined) => {
  const Wrapper = makeWrapper();
  return render(
    <Wrapper>
      <SchemaRegistryHome schemaId={schemaId} v2Cursor="now()" v2Count={0} />
    </Wrapper>,
  );
};

describe('SchemaRegistryHome', () => {
  beforeEach(() => {
    h.navigate.mockClear();
    window.history.replaceState({}, '', '/api/schema-registry');
    h.listResponse.current = {
      kind: 'success',
      response: [dump],
      totalCount: 1,
    };
    h.urlResponse.current = { kind: 'loading' };
  });

  it('defaults to the latest admin role (navigates) when no role is in the URL', () => {
    renderHome(undefined);

    // selectedRoleID derives to null for the empty URL, so the details pane is
    // not rendered yet; instead we redirect to the admin role.
    expect(
      screen.queryByTestId('schema-change-details'),
    ).not.toBeInTheDocument();
    expect(h.navigate).toHaveBeenCalledWith(
      '/api/schema-registry/admin-role-id',
    );
  });

  it('derives selectedRoleID from the URL schema id', () => {
    renderHome('user-role-id');

    expect(screen.getByTestId('schema-change-details')).toHaveTextContent(
      'user-role-id',
    );
    expect(h.navigate).not.toHaveBeenCalled();
  });

  it('does not render the details pane for the empty uuid sentinel', () => {
    renderHome(EMPTY_UUID_STRING);
    expect(
      screen.queryByTestId('schema-change-details'),
    ).not.toBeInTheDocument();
  });

  it('does not re-render in a loop', () => {
    let renderCount = 0;
    const Counting = () => {
      renderCount++;
      return (
        <SchemaRegistryHome
          schemaId="user-role-id"
          v2Cursor="now()"
          v2Count={0}
        />
      );
    };
    const Wrapper = makeWrapper();
    render(
      <Wrapper>
        <Counting />
      </Wrapper>,
    );
    expect(screen.getByTestId('schema-change-details')).toBeInTheDocument();
    expect(renderCount).toBeLessThanOrEqual(3);
  });
});

// --- SchemaChangeList / SchemaCard -----------------------------------------
const cardWith = (
  idSuffix: string,
  tags: { id: string; name: string; color: string }[],
) => ({
  hash: `hash-${idSuffix}`,
  created_at: '2023-01-01T00:00:00Z',
  id: `card-${idSuffix}`,
  entry_hash: `eh-${idSuffix}`,
  roles: [
    { id: `admin-${idSuffix}`, role: 'admin' },
    { id: `${idSuffix}2`, role: `${idSuffix}2` },
    { id: `${idSuffix}3`, role: `${idSuffix}3` },
    { id: `${idSuffix}4`, role: `${idSuffix}4` },
  ],
  tags,
});

const renderList = (schemas: any[]) => {
  const Wrapper = makeWrapper();
  return render(
    <Wrapper>
      <SchemaChangeList
        schemas={schemas}
        urlSchemaCard={[]}
        pageNumber={0}
        totalCount={schemas.length}
        selectedRoleID={null}
        handlePrev={vi.fn()}
        handleNext={vi.fn()}
      />
    </Wrapper>,
  );
};

describe('SchemaCard (via SchemaChangeList)', () => {
  beforeEach(() => {
    h.navigate.mockClear();
  });

  it('derives the open state from the opened card index', () => {
    // Two cards with 4 roles each (unique role names per card). Card index 0 is
    // open by default (the list initialises openSchemaCardIndex to 0) so its 4th
    // role "A4" is visible while the collapsed card b only shows its first three
    // roles (B4 hidden).
    renderList([
      cardWith('a', [{ id: 't-a', name: 'v1', color: '#36A7E6' }]),
      cardWith('b', [{ id: 't-b', name: 'v2', color: '#36A7E6' }]),
    ]);

    expect(screen.getByText('A4')).toBeInTheDocument();
    expect(screen.queryByText('B4')).not.toBeInTheDocument();

    // Open card b -> its derived open state flips (B4 shows); card a collapses.
    fireEvent.click(
      screen.getAllByRole('button', { name: /View schema published/ })[1],
    );
    expect(screen.getByText('B4')).toBeInTheDocument();
    expect(screen.queryByText('A4')).not.toBeInTheDocument();
  });

  it('does not mutate the supplied role order', () => {
    const card = cardWith('a', []);
    card.roles.reverse();
    const originalOrder = card.roles.map((role) => role.id);
    renderList([card]);
    expect(card.roles.map((role) => role.id)).toEqual(originalOrder);
  });

  it('renders a deep-linked role even when its card would otherwise be collapsed', () => {
    const Wrapper = makeWrapper();
    render(
      <Wrapper>
        <SchemaChangeList
          schemas={[cardWith('a', []), cardWith('b', [])]}
          urlSchemaCard={[]}
          pageNumber={0}
          totalCount={2}
          selectedRoleID="b4"
          handlePrev={vi.fn()}
          handleNext={vi.fn()}
        />
      </Wrapper>,
    );
    expect(screen.getByRole('button', { name: 'B4' })).toHaveAttribute(
      'aria-current',
      'true',
    );
  });

  it('deduplicates a deep-linked schema already on the page', () => {
    const Wrapper = makeWrapper();
    const card = cardWith('a', []);
    render(
      <Wrapper>
        <SchemaChangeList
          schemas={[card]}
          urlSchemaCard={[card]}
          pageNumber={0}
          totalCount={1}
          selectedRoleID="admin-a"
          handlePrev={vi.fn()}
          handleNext={vi.fn()}
        />
      </Wrapper>,
    );
    expect(screen.getAllByRole('button', { name: 'Admin' })).toHaveLength(1);
    expect(screen.getByRole('button', { name: 'Prev' })).toBeDisabled();
    expect(screen.getByRole('button', { name: 'Next' })).toBeDisabled();
  });

  it('re-syncs the tags list when the tags prop changes', () => {
    const { rerender } = renderList([
      cardWith('a', [{ id: 't-a', name: 'v1', color: '#36A7E6' }]),
    ]);

    expect(screen.getByText('v1')).toBeInTheDocument();

    const Wrapper = makeWrapper();
    rerender(
      <Wrapper>
        <SchemaChangeList
          schemas={[
            cardWith('a', [{ id: 't-b', name: 'v2', color: '#36A7E6' }]),
          ]}
          urlSchemaCard={[]}
          pageNumber={0}
          totalCount={1}
          selectedRoleID={null}
          handlePrev={vi.fn()}
          handleNext={vi.fn()}
        />
      </Wrapper>,
    );

    expect(screen.getByText('v2')).toBeInTheDocument();
    expect(screen.queryByText('v1')).not.toBeInTheDocument();
  });
});
