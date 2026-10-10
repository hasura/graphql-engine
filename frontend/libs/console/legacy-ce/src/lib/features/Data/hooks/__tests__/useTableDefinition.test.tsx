import { renderHook } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import {
  QueryStringParseResult,
  useTableDefinition,
} from '../useTableDefinition';

// useTableDefinition reads the URL via react-router's useSearchParams (and
// useParams), both of which require a <Router> ancestor, so the hook must be
// rendered inside one (see MemoryRouter usage in sibling tests such as
// ManageTable/parts/TableName.test.tsx). Note that useSearchParams resolves
// against the router's own location, not `window.location`, so the mocked
// URL below must be supplied via MemoryRouter's `initialEntries` rather than
// (only) `Object.defineProperty(window, 'location', ...)`.
const makeWrapper =
  (initialEntries?: string[]) =>
  ({ children }: { children: React.ReactNode }) => (
    <MemoryRouter initialEntries={initialEntries}>{children}</MemoryRouter>
  );

global.window = Object.create(window);
describe('useTableDefinition', () => {
  it('should give error when there is no table definition in the URL', async () => {
    const { result } = renderHook(() => useTableDefinition(), {
      wrapper: makeWrapper(),
    });
    expect(result.current?.querystringParseResult).toBe('error');
  });

  it('should parse correct table definition from window object', async () => {
    // mock window.location
    const url = new URL(
      'http://localhost:3000/console/data/v2?database=gdc_demo_database&table=%7B%22schema%22:%22baz%22,%22anotherSchema%22:%22bar%22,%22name%22:%22Employee%22%7D',
    );
    Object.defineProperty(window, 'location', {
      value: {
        href: url.href,
        search: url.search,
      },
    });

    const { result } = renderHook(() => useTableDefinition(), {
      wrapper: makeWrapper([`${url.pathname}${url.search}`]),
    });
    const data: QueryStringParseResult = result.current;
    expect(data).toStrictEqual({
      querystringParseResult: 'success',
      data: {
        database: 'gdc_demo_database',
        // No `schema`/`table`/`operation` route params are matched (the
        // MemoryRouter above has no <Routes>), so useParams() yields
        // `undefined` for `operation` and the `schema` fallback chain
        // (`params.schema || searchParams.get('schema')`) yields `null`
        // since there's no `schema` search param either.
        schema: null,
        operation: undefined,
        table: {
          anotherSchema: 'bar',
          name: 'Employee',
          schema: 'baz',
        },
      },
    });
  });
});
