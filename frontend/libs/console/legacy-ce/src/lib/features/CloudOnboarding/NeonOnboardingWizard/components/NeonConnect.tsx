import { useNavigate } from 'react-router';
import { useInvalidateMetadata, useMetadata } from '@hasura/metadata/api';
import { FETCH_NEON_PROJECTS_BY_PROJECTID_QUERYKEY } from './NeonDashboardLink';
import { useNeonIntegration } from '../hooks/useNeonIntegration';
import {
  getNeonDBName,
  transformNeonIntegrationStatusToNeonBannerProps,
} from '../hooks/utils';
import { NeonBanner } from './NeonConnectBanner/NeonBanner';
import { useQueryClient } from '@tanstack/react-query';

type NeonConnectProps = {
  connectDbUrl?: string;
};
// This component deals with Neon DB creation on connect DB page
export function NeonConnect({
  connectDbUrl = '/data/manage/connect',
}: NeonConnectProps) {
  const navigate = useNavigate();
  const queryClient = useQueryClient();
  const { data } = useMetadata();
  const invalidateMetadata = useInvalidateMetadata();
  const allDatabases =
    data?.metadata.sources.map((source) => source.name) ?? [];

  // success callback
  const pushToDataSource = (dataSourceName: string) => {
    // on success, refetch queries to show neon dashboard link in connect database page,
    // overriding the stale time
    queryClient.refetchQueries({
      queryKey: FETCH_NEON_PROJECTS_BY_PROJECTID_QUERYKEY,
    });

    // invalidate react query metadata on success
    invalidateMetadata({
      componentName: 'NeonConnect',
      reasons: ['Successfully adding neon source.'],
    });

    navigate(`/data/${dataSourceName}/schema/public`);
  };
  const pushToConnectDBPage = () => {
    navigate(connectDbUrl);
  };

  const neonIntegrationStatus = useNeonIntegration(
    getNeonDBName(allDatabases),
    pushToDataSource,
    pushToConnectDBPage,
    'data-manage-create',
  );

  const neonBannerProps = transformNeonIntegrationStatusToNeonBannerProps(
    neonIntegrationStatus,
  );

  return <NeonBanner {...neonBannerProps} />;
}
