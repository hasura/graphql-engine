import { extractTableInfo } from '@hasura/shared/utils';
import { useParams, useSearchParams } from 'react-router';

const useDatabaseTableParams = () => {
  const params = useParams();
  const [searchParams] = useSearchParams();

  const tableInfo = tryParseTableInfo(searchParams.get('table'));
  const schema = searchParams.get('schema');

  return {
    source: params.source,
    schema: tableInfo?.schema || schema,
    tableName: tableInfo?.name,
  };
};

export type DatabaseTableParamsReturn = ReturnType<
  typeof useDatabaseTableParams
>;

const tryParseTableInfo = (value: string | null) => {
  if (!value) {
    return null;
  }

  try {
    const table = JSON.parse(value);
    if (!table) {
      return null;
    }

    return extractTableInfo(table);
  } catch {
    return null;
  }
};

export default useDatabaseTableParams;
