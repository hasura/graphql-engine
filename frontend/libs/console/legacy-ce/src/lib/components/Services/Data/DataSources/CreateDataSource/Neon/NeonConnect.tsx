import {
  useInvalidateMetadata,
  useMetadata,
} from '../../../../../../features/hasura-metadata-api';
import { reactQueryClient } from '../../../../../../lib/reactQuery';
import { Dispatch } from '../../../../../../types';
import _push from '../../../push';
import { NeonBanner } from './components/Neon/NeonBanner';
import { FETCH_NEON_PROJECTS_BY_PROJECTID_QUERYKEY } from './components/NeonDashboardLink';
import { useNeonIntegration } from './useNeonIntegration';
import {
  getNeonDBName,
  transformNeonIntegrationStatusToNeonBannerProps,
} from './utils';

type NeonConnectProps = {
  dispatch: Dispatch;
  connectDbUrl?: string;
};
// This component deals with Neon DB creation on connect DB page
export function NeonConnect({
  dispatch,
  connectDbUrl = '/data/manage/connect',
}: NeonConnectProps) {
  const { data } = useMetadata();
  const invalidateMetadata = useInvalidateMetadata();
  const allDatabases = data?.metadata.sources.map(source => source.name) ?? [];

  // success callback
  const pushToDataSource = (dataSourceName: string) => {
    // on success, refetch queries to show neon dashboard link in connect database page,
    // overriding the stale time
    reactQueryClient.refetchQueries(FETCH_NEON_PROJECTS_BY_PROJECTID_QUERYKEY);

    // invalidate react query metadata on success
    invalidateMetadata({
      componentName: 'NeonConnect',
      reasons: ['Successfully adding neon source.'],
    });

    dispatch(_push(`/data/${dataSourceName}/schema/public`));
  };
  const pushToConnectDBPage = () => {
    dispatch(_push(connectDbUrl));
  };

  const neonIntegrationStatus = useNeonIntegration(
    getNeonDBName(allDatabases),
    pushToDataSource,
    pushToConnectDBPage,
    dispatch,
    'data-manage-create'
  );

  const neonBannerProps = transformNeonIntegrationStatusToNeonBannerProps(
    neonIntegrationStatus
  );

  return <NeonBanner {...neonBannerProps} />;
}
