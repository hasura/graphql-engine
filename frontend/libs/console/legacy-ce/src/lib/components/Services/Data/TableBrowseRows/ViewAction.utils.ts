import { UserQuery } from '../../../../features/BrowseRows';

type NestedColumn = {
  columns: (string | NestedColumn)[];
  limit: number;
  name: string;
  offset: number;
  order_by: UserQuery['order_by'];
  where: UserQuery['where'];
};

export type QueryState = {
  query: {
    columns: (string | NestedColumn)[];
    limit: number;
    name?: string;
    oldStuff?: unknown;
    where: any;
  };
};

export const updateLimit = (
  state: QueryState,
  limit: number,
  curPath: string[]
): QueryState => {
  if (!curPath || curPath.length === 0) {
    return {
      ...state,
      query: {
        ...state.query,
        limit,
      },
    };
  }

  if (curPath && curPath.length > 0) {
    let baseColumns = state.query.columns;
    curPath.forEach((path, index) => {
      const colIndex = baseColumns.findIndex(
        col => typeof col !== 'string' && col?.name && col?.name === path
      );

      if (index === curPath.length - 1) {
        const column = baseColumns[colIndex];
        if (typeof column !== 'string') {
          column.limit = limit;
        }
        return;
      }

      const col = baseColumns[colIndex];
      if (colIndex !== -1 && typeof col !== 'string') {
        baseColumns = col.columns;
      }
    });
    return {
      ...state,
    };
  }

  return state;
};
