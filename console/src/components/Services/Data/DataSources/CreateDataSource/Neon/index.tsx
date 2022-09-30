import * as React from 'react';
import { Dispatch } from '@/types';
import { NeonBanner } from './components/Neon/NeonBanner';
import {
  getNeonDBName,
  transformNeonIntegrationStatusToNeonBannerProps,
} from './utils';
import { useNeonIntegration } from './useNeonIntegration';
import _push from '../../../push';

// This component deals with Neon DB creation on connect DB page
export function Neon(props: { allDatabases: string[]; dispatch: Dispatch }) {
  const { dispatch, allDatabases } = props;

  const pushToDatasource = (dataSourceName: string) => {
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
