import * as React from 'react';
import { Dispatch } from '@/types';
import { reactQueryClient } from '@/lib/reactQuery';
import { NeonBanner } from './components/Neon/NeonBanner';
import {
  getNeonDBName,
  transformNeonIntegrationStatusToNeonBannerProps,
} from './utils';
import { useNeonIntegration } from './useNeonIntegration';
import _push from '../../../push';
import { FETCH_NEON_PROJECTS_BY_PROJECTID_QUERYKEY } from './components/NeonDashboardLink';

// This component deals with Neon DB creation on connect DB page
export function Neon(props: { allDatabases: string[]; dispatch: Dispatch }) {
  const { dispatch, allDatabases } = props;

  // success callback
  const pushToDatasource = (dataSourceName: string) => {
    // on success, refetch queries to show neon onboarding link in connect database page,
    // overriding the stale time
    reactQueryClient.refetchQueries(FETCH_NEON_PROJECTS_BY_PROJECTID_QUERYKEY);

    dispatch(_push(`/data/${dataSourceName}`));
  };
  const pushToConnectDBPage = () => {
    dispatch(_push(`/data/manage/connect`));
  };

  const neonIntegrationStatus = useNeonIntegration(
    getNeonDBName(allDatabases),
    pushToDatasource,
    pushToConnectDBPage,
    dispatch,
    'data-manage-create'
  );

  const neonBannerProps = transformNeonIntegrationStatusToNeonBannerProps(
    neonIntegrationStatus
  );

  return <NeonBanner {...neonBannerProps} />;
}

export { useNeonIntegration } from './useNeonIntegration';
