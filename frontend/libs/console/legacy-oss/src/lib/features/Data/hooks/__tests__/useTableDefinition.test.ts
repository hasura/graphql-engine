import { renderHook } from '@testing-library/react-hooks';
import {
  QueryStringParseResult,
  useTableDefinition,
} from '../useTableDefinition';

global.window = Object.create(window);
describe('useTableDefinition', () => {
  it('should give error when there is no table definition in the URL', async () => {
    const { result } = renderHook(() => useTableDefinition());
    expect(result.current?.querystringParseResult).toBe('error');
  });

  it('should parse correct table definition from window object', async () => {
    // mock window.location
    const url = new URL(
      'http://localhost:3000/console/data/v2?database=gdc_demo_database&table=%7B%22schema%22:%22baz%22,%22anotherSchema%22:%22bar%22,%22name%22:%22Employee%22%7D'
    );
    Object.defineProperty(window, 'location', {
      value: {
        href: url.href,
        search: url.search,
      },
    });

    const { result } = renderHook(() => useTableDefinition());
    const data: QueryStringParseResult = result.current;
    expect(data).toStrictEqual({
      querystringParseResult: 'success',
      data: {
        database: 'gdc_demo_database',
        table: {
          anotherSchema: 'bar',
          name: 'Employee',
          schema: 'baz',
        },
      },
    });
  });
});
