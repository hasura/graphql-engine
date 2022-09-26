import * as React from 'react';
import { Dispatch } from '@/types';
import {
  NeonBanner,
  Props as NeonBannerProps,
} from './components/Neon/NeonBanner';
import { getNeonDBName } from './utils';
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
    dispatch
  );

  let neonBannerProps: NeonBannerProps;
  switch (neonIntegrationStatus.status) {
    case 'idle':
      neonBannerProps = {
        status: {
          status: 'default',
        },
        buttonText: 'Connect Neon Database',
        onClickConnect: neonIntegrationStatus.action,
      };
      break;
    case 'authentication-loading':
      neonBannerProps = {
        status: {
          status: 'loading',
        },
        buttonText: 'Authenticating with Neon',
        onClickConnect: () => null,
      };
      break;
    case 'authentication-error':
      neonBannerProps = {
        status: {
          status: 'error',
          errorTitle: neonIntegrationStatus.title,
          errorDescription: neonIntegrationStatus.description,
        },
        buttonText: 'Try again',
        onClickConnect: neonIntegrationStatus.action,
        icon: 'refresh',
      };
      break;
    case 'authentication-success':
    case 'neon-database-creation-loading':
      neonBannerProps = {
        status: {
          status: 'loading',
        },
        buttonText: 'Creating Database',
        onClickConnect: () => null,
      };
      break;
    case 'neon-database-creation-error':
      neonBannerProps = {
        status: {
          status: 'error',
          errorTitle: neonIntegrationStatus.title,
          errorDescription: neonIntegrationStatus.description,
        },
        buttonText: 'Try again',
        onClickConnect: neonIntegrationStatus.action,
        icon: 'refresh',
      };
      break;
    case 'neon-database-creation-success':
    case 'env-var-creation-loading':
    case 'env-var-creation-success':
    case 'env-var-creation-error':
    case 'hasura-source-creation-loading':
    case 'hasura-source-creation-error':
    case 'hasura-source-creation-success':
      neonBannerProps = {
        status: {
          status: 'loading',
        },
        buttonText: 'Connecting to Hasura',
        onClickConnect: () => null,
      };
      break;
    default:
      neonBannerProps = {
        status: {
          status: 'default',
        },
        buttonText: 'Connect Neon Database',
        onClickConnect: () => null,
      };
      break;
  }

  return <NeonBanner {...neonBannerProps} />;
}
