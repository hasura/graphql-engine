import { AxiosInstance } from 'axios';
import { useCallback } from 'react';
import { PostgresTable, getDriverPrefix } from '../../../../DataSource';
import { useHttpClient } from '../../../../Network';
import {
  MetadataSelectors,
  runMetadataQuery,
  useMetadata,
} from '../../../../hasura-metadata-api';
import { useConsoleQuery } from '../../../reactQueryUtils';
import { getTrackedSuggestedRelationships } from '../selectors/selectors';
import {
  SuggestedRelationship,
  SuggestedRelationshipWithName,
  SuggestedRelationshipsResponse,
  TrackedSuggestedRelationship,
} from '../types';
import { addConstraintName } from '../utils';
import { useQueryClient } from 'react-query';

// hook return type:
type QueryReturnType = {
  all?: SuggestedRelationship[];
  untracked?: SuggestedRelationship[];
};

type SelectReturnType = {
  untracked?: SuggestedRelationshipWithName[];
  tracked?: TrackedSuggestedRelationship[];
};

// since we have to do this a few times, putting into a simple re-usable function
const runQuery = async ({
  prefix,
  dataSourceName,
  omit_tracked,
  httpClient,
}: {
  prefix: string;
  dataSourceName: string;
  omit_tracked: boolean;
  httpClient: AxiosInstance;
}) =>
  runMetadataQuery<SuggestedRelationshipsResponse>({
    httpClient,
    body: {
      type: `${prefix}_suggest_relationships`,
      args: {
        omit_tracked,
        source: dataSourceName,
      },
    },
  });

const QUERY_KEY = 'suggested_relationships' as const;

export const useInvalidateSuggestedRelationships = ({
  dataSourceName,
}: {
  dataSourceName: string;
}) => {
  const client = useQueryClient();
  const invalidateSuggestedRelationships = useCallback(
    () => client.invalidateQueries([dataSourceName, QUERY_KEY]),
    [client, dataSourceName]
  );
  return { invalidateSuggestedRelationships };
};

const filterBySchema = (schema: string, rel: SuggestedRelationship) =>
  (rel.from.table as PostgresTable).schema === schema;

export const useSuggestedRelationships = ({
  dataSourceName,
  which,
  schema,
}: {
  dataSourceName: string;
  which: 'tracked' | 'untracked' | 'all';
  schema?: string;
}) => {
  const httpClient = useHttpClient();

  const { data: { source, fkRels = [] } = {}, isFetching } = useMetadata(m => {
    return {
      fkRels: MetadataSelectors.getForeignKeyRelationships(dataSourceName)(m),
      source: MetadataSelectors.findSource(dataSourceName)(m),
    };
  });

  const selector = useCallback(
    (data: QueryReturnType) => {
      const rData: SelectReturnType = {};

      if (data.all) {
        // if schema was passed in, filter by schema
        const rels = schema
          ? data.all.filter(rel => filterBySchema(schema, rel))
          : data.all;
        rData.tracked = getTrackedSuggestedRelationships({
          fkConstraintRelationships: fkRels,
          suggestedRelationships: rels,
        });
      }

      if (data.untracked) {
        const rels = schema
          ? data.untracked.filter(rel => filterBySchema(schema, rel))
          : data.untracked;
        rData.untracked = addConstraintName({
          namingConvention:
            source?.customization?.naming_convention ?? 'hasura-default',
          relationships: rels,
        });
      }

      return rData;
    },
    [fkRels, schema, source?.customization?.naming_convention]
  );

  const { invalidateSuggestedRelationships: invalidateQuery } =
    useInvalidateSuggestedRelationships({
      dataSourceName,
    });

  const query = useConsoleQuery<QueryReturnType, SelectReturnType>({
    queryKey: [dataSourceName, QUERY_KEY],
    select: selector,
    enabled: !isFetching,
    queryFn: async () => {
      if (!source)
        throw Error(`Unable to find source, "${dataSourceName}" in metadata`);

      const prefix = getDriverPrefix(source.kind);

      const returnData: QueryReturnType = {};

      if (which === 'tracked' || which === 'all') {
        const all = await runQuery({
          prefix,
          dataSourceName,
          omit_tracked: false,
          httpClient,
        });

        returnData.all = all.relationships;
      }

      if (which === 'untracked' || which === 'all') {
        const untracked = await runQuery({
          prefix,
          dataSourceName,
          omit_tracked: true,
          httpClient,
        });
        returnData.untracked = untracked.relationships;
      }

      return returnData;
    },
  });

  return { ...query, invalidateQuery };
};
