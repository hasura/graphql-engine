import { useCallback } from 'react';
import { getTrackedSuggestedRelationships } from './selectors/selectors';
import {
  SuggestedRelationship,
  SuggestedRelationshipWithName,
  SuggestedRelationshipsResponse,
  TrackedSuggestedRelationship,
} from './types';
import { addConstraintName } from './utils';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { FetchJson } from '@hasura/shared/utils';
import { useAppContext } from '@hasura/shared/context';
import { getDriverPrefix } from '@hasura/metadata/helpers';
import { QualifiedTable, SupportedDriver } from '@hasura/shared/types';
import { HttpError } from '@hasura/shared/types';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import { runMetadataQuery } from '../../../api';
import { useMetadata } from '../../metadata';

export * from './types';

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
  url,
  driver,
  dataSourceName,
  omit_tracked,
  fetchJson,
}: {
  url: string;
  driver: SupportedDriver;
  dataSourceName: string;
  omit_tracked: boolean;
  fetchJson: FetchJson;
}) => {
  const prefix = getDriverPrefix(driver);

  return runMetadataQuery<SuggestedRelationshipsResponse>({
    url,
    fetchJson,
    body: {
      type: `${prefix}_suggest_relationships` as const,
      args: {
        omit_tracked,
        source: dataSourceName,
      },
    },
  });
};

const QUERY_KEY = 'suggested_relationships' as const;

export const useInvalidateSuggestedRelationships = ({
  dataSourceName,
}: {
  dataSourceName: string;
}) => {
  const client = useQueryClient();
  const invalidateSuggestedRelationships = useCallback(
    () => client.invalidateQueries({ queryKey: [dataSourceName, QUERY_KEY] }),
    [client, dataSourceName],
  );
  return invalidateSuggestedRelationships;
};

const filterBySchema = (schema: string, rel: SuggestedRelationship) =>
  (rel.from.table as QualifiedTable).schema === schema;

export const useSuggestedRelationships = ({
  dataSourceName,
  which,
  schema,
}: {
  dataSourceName: string;
  which: 'tracked' | 'untracked' | 'all';
  schema?: string;
}) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  const { data: { source, fkRels = [] } = {}, isFetching } = useMetadata(
    (m) => {
      return {
        fkRels:
          MetadataSelectors.selectForeignKeyRelationships(dataSourceName)(m),
        source: MetadataSelectors.findSource(dataSourceName)(m),
      };
    },
  );

  const selector = (data: QueryReturnType) => {
    const rData: SelectReturnType = {};

    if (data.all) {
      // if schema was passed in, filter by schema
      const rels = schema
        ? data.all.filter((rel) => filterBySchema(schema, rel))
        : data.all;
      rData.tracked = getTrackedSuggestedRelationships({
        fkConstraintRelationships: fkRels,
        suggestedRelationships: rels,
      });
    }

    if (data.untracked) {
      const rels = schema
        ? data.untracked.filter((rel) => filterBySchema(schema, rel))
        : data.untracked;
      rData.untracked = addConstraintName({
        namingConvention:
          source?.customization?.naming_convention ?? 'hasura-default',
        relationships: rels,
      });
    }

    return rData;
  };

  const query = useQuery<QueryReturnType, HttpError, SelectReturnType>({
    queryKey: [QUERY_KEY, dataSourceName],
    select: selector,
    enabled: !isFetching,
    queryFn: async () => {
      if (!source)
        throw Error(`Unable to find source, "${dataSourceName}" in metadata`);

      const returnData: QueryReturnType = {};

      if (which === 'tracked' || which === 'all') {
        const all = await runQuery({
          url: endpoints.metadata,
          driver: source.kind,
          dataSourceName,
          omit_tracked: false,
          fetchJson,
        });

        returnData.all = all.relationships;
      }

      if (which === 'untracked' || which === 'all') {
        const untracked = await runQuery({
          url: endpoints.metadata,
          driver: source.kind,
          dataSourceName,
          omit_tracked: true,
          fetchJson,
        });
        returnData.untracked = untracked.relationships;
      }

      return returnData;
    },
  });

  return query;
};
