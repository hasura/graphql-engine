import { useQuery } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import {
  QualifiedDataSource,
  Table,
  TableFunction,
} from '@hasura/shared/types';
import { getDatabaseMethods } from '../../driver';

export type DatabaseCommentTarget =
  | { type: 'table'; table: Table }
  | { type: 'view'; table: Table }
  | { type: 'function'; func: TableFunction };

const GET_DATABASE_COMMENT_QUERY_KEY = 'GET_DATABASE_COMMENT';

const getCommentMethod = (
  kind: QualifiedDataSource['kind'],
  type: DatabaseCommentTarget['type'],
) => {
  const { introspection } = getDatabaseMethods(kind);
  switch (type) {
    case 'table':
      return introspection.getTableComment;
    case 'view':
      return introspection.getViewComment;
    case 'function':
      return introspection.getFunctionComment;
  }
};

/** Whether the driver can read the database (SQL) comment of `type`. */
export const isDatabaseCommentSupported = (
  kind: QualifiedDataSource['kind'],
  type: DatabaseCommentTarget['type'],
) => Boolean(getCommentMethod(kind, type));

/**
 * Reads the database (SQL) comment of a table, view or function on demand.
 * Console comments live in metadata `configuration.comment`; this only exists
 * to import the database comment, so the query never runs on its own: call
 * `fetchComment()` (e.g. from a button).
 */
export const useDatabaseComment = ({
  source,
  target,
}: {
  source: QualifiedDataSource;
  target: DatabaseCommentTarget;
}) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const isSupported = isDatabaseCommentSupported(source.kind, target.type);

  const { refetch, isFetching } = useQuery({
    queryKey: [source.name, GET_DATABASE_COMMENT_QUERY_KEY, target],
    queryFn: async (): Promise<string | null> => {
      const network = { dataSourceName: source.name, endpoints, fetchJson };
      const { introspection } = getDatabaseMethods(source.kind);
      let comment: string | undefined;
      if (target.type === 'function') {
        comment = await introspection.getFunctionComment?.({
          ...network,
          func: target.func,
        });
      } else {
        const getComment =
          target.type === 'view'
            ? introspection.getViewComment
            : introspection.getTableComment;
        comment = await getComment?.({ ...network, table: target.table });
      }
      // react-query doesn't allow `undefined` query data
      return comment === 'NULL' ? '' : (comment ?? null);
    },
    enabled: false,
    retry: false,
  });

  const fetchComment = async (): Promise<string | undefined> => {
    const { data } = await refetch({ throwOnError: true });
    return data ?? undefined;
  };

  return { isSupported, fetchComment, isFetching };
};
