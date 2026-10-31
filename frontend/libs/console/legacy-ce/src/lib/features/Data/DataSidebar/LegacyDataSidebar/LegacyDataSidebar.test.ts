import { matchesAny, ROUTE_PATTERNS } from '.';

const activeItems = (pathname: string) =>
  Object.entries(ROUTE_PATTERNS)
    .filter(([, patterns]) => matchesAny(patterns, pathname))
    .map(([item]) => item);

describe('LegacyDataSidebar active item', () => {
  it.each`
    pathname                                                      | item
    ${'/data/manage'}                                             | ${'manage'}
    ${'/data/manage/connect'}                                     | ${'manage'}
    ${'/data/manage/source/default'}                              | ${'manage'}
    ${'/data/manage/source/default/table/browse'}                 | ${'manage'}
    ${'/data/manage/source/default/function/modify'}              | ${'manage'}
    ${'/data/edit'}                                               | ${'manage'}
    ${'/data/schema/manage'}                                      | ${'manage'}
    ${'/data/v2/manage/table/browse'}                             | ${'manage'}
    ${'/data/default/schema/public/tables/users/browse'}          | ${'manage'}
    ${'/data/sql'}                                                | ${'sql'}
    ${'/data/native-queries'}                                     | ${'nativeQueries'}
    ${'/data/native-queries/logical-models/default/user/details'} | ${'nativeQueries'}
    ${'/data/native-queries/default/get_users/details'}           | ${'nativeQueries'}
    ${'/data/model-count-summary'}                                | ${'modelSummary'}
    ${'/data/migrations'}                                         | ${'migrations'}
  `('activates only $item for $pathname', ({ pathname, item }) => {
    expect(activeItems(pathname)).toEqual([item]);
  });

  it.each([
    // names that contain another item's path segment
    '/data/manage/source/sqlite/table/browse',
    '/data/manage/source/native-queries/table/browse',
    '/data/manage/source/migrations',
  ])('activates only the Data Manager for %s', (pathname) => {
    expect(activeItems(pathname)).toEqual(['manage']);
  });

  it.each(['/data/sqlite', '/settings/data/manage', '/datamanage'])(
    'activates nothing for %s',
    (pathname) => {
      expect(activeItems(pathname)).toEqual([]);
    },
  );
});
