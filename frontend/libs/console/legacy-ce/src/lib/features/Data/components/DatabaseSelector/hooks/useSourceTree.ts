import { useSources } from '../../../../MetadataAPI';
import { useMemo } from 'react';

export const useSourcesTree = () => {
  const { data, isLoading } = useSources();

  const tree = useMemo(() => {
    if (!data) return {};

    const temp = data.reduce((databases, source) => {
      return {
        ...databases,
        [source.name]: {
          kind: source.kind,
          children: source.tables.reduce((schemas, { table }) => {
            const key = source.kind === 'bigquery' ? 'dataset' : 'schema';
            const child = table.schema ?? (table as any).dataset;

            return {
              ...schemas,
              [child]: source.tables
                .filter((t: any) => t.table[key] === child)
                .map(t => t.table.name),
            };
          }, {}),
        },
      };
    }, {});

    return temp as any;
  }, [data]);

  return { tree, isLoading };
};
