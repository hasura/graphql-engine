import {
  getLSItem,
  LS_KEYS,
  removeLSItem,
  setLSItem,
} from '../../../../../../utils/localStorage';
import { NeonIntegrationStatus } from './useNeonIntegration';
import type { Props as NeonBannerProps } from './components/Neon/NeonBanner';

export const NEON_CALLBACK_SEARCH = LS_KEYS.neonCallbackSearch;

export const clearPersistedNeonCallbackSearch = () => {
  removeLSItem(NEON_CALLBACK_SEARCH);
};

export const persistNeonCallbackSearch = (value: string) => {
  setLSItem(NEON_CALLBACK_SEARCH, value);
};

export const getPersistedNeonCallbackSearch = () => {
  return getLSItem(NEON_CALLBACK_SEARCH);
};

export function getNeonDBName(allDatabases: string[]) {
  if (!allDatabases.includes('default')) {
    return 'default';
  }

  const prefix = 'neon-db';
  let suffix = 0;
  let dbName = prefix;
  while (allDatabases.includes(dbName)) {
    dbName = `${prefix}-${++suffix}`;
  }

  return dbName;
}

export type NeonIntegrationContext = 'onboarding' | 'data-manage-create';

export function transformNeonIntegrationStatusToNeonBannerProps(
  neonIntegrationStatus: NeonIntegrationStatus
): NeonBannerProps {
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
    case 'hasura-source-creation-success':
      neonBannerProps = {
        status: {
          status: 'loading',
        },
        buttonText: 'Connecting to Hasura',
        onClickConnect: () => null,
      };
      break;
    case 'hasura-source-creation-error':
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
  return neonBannerProps;
}
