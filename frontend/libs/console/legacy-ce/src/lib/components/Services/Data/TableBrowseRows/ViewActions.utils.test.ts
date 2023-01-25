import { updateLimit, QueryState } from './ViewAction.utils';

describe('updateLimit', () => {
  it('set limit', () => {
    const viewState: QueryState = {
      query: {
        columns: [],
        limit: 0,
        where: {},
        name: undefined,
      },
    };
    expect(updateLimit(viewState, 5, [])).toEqual({
      ...viewState,
      query: {
        ...viewState.query,
        limit: 5,
      },
    });
  });

  it('set limit for path', () => {
    const viewState: QueryState = {
      query: {
        columns: [
          'id',
          {
            columns: [],
            limit: 5,
            name: 'posts',
            offset: 0,
            order_by: [],
            where: { $and: [] },
          },
        ],
        limit: 0,
        where: {},
        name: undefined,
      },
    };
    expect(updateLimit(viewState, 10, ['posts'])).toEqual({
      ...viewState,
      query: {
        ...viewState.query,
        columns: [
          'id',
          {
            columns: [],
            limit: 10,
            name: 'posts',
            offset: 0,
            order_by: [],
            where: { $and: [] },
          },
        ],
      },
    });
  });

  it('set limit for nested paths', () => {
    const viewState: QueryState = {
      query: {
        columns: [
          'id',
          {
            columns: [
              'id',
              {
                columns: [],
                limit: 5,
                name: 'images',
                offset: 0,
                order_by: [],
                where: { $and: [] },
              },
            ],
            limit: 5,
            name: 'posts',
            offset: 0,
            order_by: [],
            where: { $and: [] },
          },
        ],
        limit: 0,
        where: {},
        name: undefined,
      },
    };
    expect(updateLimit(viewState, 10, ['posts', 'images'])).toEqual({
      query: {
        columns: [
          'id',
          {
            columns: [
              'id',
              {
                columns: [],
                limit: 10,
                name: 'images',
                offset: 0,
                order_by: [],
                where: { $and: [] },
              },
            ],
            limit: 5,
            name: 'posts',
            offset: 0,
            order_by: [],
            where: { $and: [] },
          },
        ],
        limit: 0,
        where: {},
        name: undefined,
      },
    });
  });

  it('set limit for 3 nested paths', () => {
    const viewState: QueryState = {
      query: {
        columns: [
          'id',
          {
            columns: [
              'id',
              {
                columns: [
                  'id',
                  {
                    columns: [],
                    limit: 20,
                    name: 'users',
                    offset: 0,
                    order_by: [],
                    where: { $and: [] },
                  },
                ],
                limit: 5,
                name: 'images',
                offset: 0,
                order_by: [],
                where: { $and: [] },
              },
            ],
            limit: 5,
            name: 'posts',
            offset: 0,
            order_by: [],
            where: { $and: [] },
          },
        ],
        limit: 0,
        where: {},
        name: undefined,
      },
    };
    expect(updateLimit(viewState, 10, ['posts', 'images', 'users'])).toEqual({
      query: {
        columns: [
          'id',
          {
            columns: [
              'id',
              {
                columns: [
                  'id',
                  {
                    columns: [],
                    limit: 10,
                    name: 'users',
                    offset: 0,
                    order_by: [],
                    where: { $and: [] },
                  },
                ],
                limit: 5,
                name: 'images',
                offset: 0,
                order_by: [],
                where: { $and: [] },
              },
            ],
            limit: 5,
            name: 'posts',
            offset: 0,
            order_by: [],
            where: { $and: [] },
          },
        ],
        limit: 0,
        where: {},
        name: undefined,
      },
    });
  });
});
