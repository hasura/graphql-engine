import globals from '../../../../../Globals';
import { OnboardingSampleDBCohortConfig } from './ReduxState';
import { Thunk } from '../../../../../types';
import {
  trackTryButtonClickEvent,
  trackConnectButtonClickEvent,
  trackDBConnectionStatusEvent,
  trackAnotherDBConnection,
} from './serverIO';

export type SampleDBTrial = {
  isActive: () => boolean;
  getDatabaseUrl: () => string;
  isExploringSampleDB: (getState: () => any) => boolean;
  track: {
    tryButton: () => void;
    connectButton: () => void;
    connectionStatus: (
      status: 'success' | 'error',
      usedDbUrl?: string
    ) => Thunk;
  };
  hasAddedSampleDB: (getState: () => any) => boolean;
};

// creates a sample DB Trial instance
export const newSampleDBTrial = (options: {
  consoleType: string;
  hasuraCloudProjectId: string;
  cohortConfig: OnboardingSampleDBCohortConfig | null;
}): SampleDBTrial => {
  const { consoleType, cohortConfig, hasuraCloudProjectId } = options;

  const isActive = () => {
    if (consoleType === 'cloud' && !!cohortConfig && !!hasuraCloudProjectId) {
      return cohortConfig.status === 'enabled' && !!cohortConfig.databaseUrl;
    }
    return false;
  };

  const getDatabaseUrl = () => {
    if (isActive()) {
      return cohortConfig!.databaseUrl;
    }
    return '';
  };

  const isExploringSampleDB = (getState: () => any) => {
    if (isActive()) {
      const state = getState();
      const currentDataSources = state?.metadata?.metadataObject?.sources || [];
      if (currentDataSources.length === 0) {
        return false;
      }
      if (
        currentDataSources.every(
          (d: any) =>
            d?.configuration?.connection_info?.database_url === getDatabaseUrl()
        )
      ) {
        return true;
      }
    }
    return false;
  };

  const hasAddedSampleDB = (getState: () => any) => {
    if (isActive()) {
      const currentDataSources =
        getState()?.metadata?.metadataObject?.sources || [];
      if (
        currentDataSources.some(
          (d: any) =>
            d?.configuration?.connection_info?.database_url === getDatabaseUrl()
        )
      ) {
        return true;
      }
    }
    return false;
  };

  return {
    isActive,
    getDatabaseUrl,
    isExploringSampleDB,
    hasAddedSampleDB,
    track: {
      tryButton: () => {
        if (isActive()) {
          trackTryButtonClickEvent(options.hasuraCloudProjectId);
        }
      },
      connectButton: () => {
        if (isActive()) {
          trackConnectButtonClickEvent(options.hasuraCloudProjectId);
        }
      },
      connectionStatus: (status, dbUrl) => (_, getState) => {
        if (isActive()) {
          if (dbUrl === getDatabaseUrl()) {
            // track sample DB getting added successfully
            trackDBConnectionStatusEvent(options.hasuraCloudProjectId, status);
          } else if (hasAddedSampleDB(getState)) {
            // track if another DB was added while exploring the sample DB
            trackAnotherDBConnection(options.hasuraCloudProjectId);
          }
        }
      },
    },
  };
};

export const checkNestedFieldValueInErrJson = (
  object: any,
  key: string,
  value: string
): boolean => {
  const objInstance = object || {};

  if (objInstance.constructor.name === 'Array') {
    for (let i = 0; i < objInstance.length; i++) {
      const childObj = objInstance[i];
      if (!childObj) {
        return false;
      }
      if (
        childObj.constructor.name === 'Object' ||
        childObj.constructor.name === 'Array'
      ) {
        if (checkNestedFieldValueInErrJson(childObj, key, value)) {
          return true;
        }
      }
    }
  }

  if (objInstance.constructor.name === 'Object') {
    const objFields = Object.keys(objInstance);
    for (let i = 0; i < objFields.length; i++) {
      const objField = objFields[i];
      const objValue = objInstance[objFields[i]];
      if (!objValue) {
        // eslint-disable-next-line no-continue
        continue;
      }
      if (
        objValue.constructor.name === 'Object' ||
        objValue.constructor.name === 'Array'
      ) {
        if (checkNestedFieldValueInErrJson(objValue, key, value)) {
          return true;
        }
      } else if (objField === key && objValue === value) {
        return true;
      }
    }
  }

  return false;
};

export const maskedErrorMessage = `You're currently connected to a read-only sample database, mutations and changes to data structure are not allowed. Please create or connect your own database to unlock all the features of Hasura.`;
export const maskPostgresError = (
  errorJson: any,
  getState: () => any,
  consoleType = globals.consoleType
): null | string => {
  if (consoleType !== 'cloud') {
    return null;
  }

  const returnMaskedError = () => {
    const sampleDBTrialService = newSampleDBTrial({
      consoleType,
      hasuraCloudProjectId: globals.hasuraCloudProjectId || '',
      cohortConfig: getState().main.cloud.onboardingSampleDB?.cohortConfig,
    });
    if (
      sampleDBTrialService.isActive() &&
      sampleDBTrialService.isExploringSampleDB(getState)
    ) {
      return maskedErrorMessage;
    }
    return null;
  };

  if (checkNestedFieldValueInErrJson(errorJson, 'status_code', '42501')) {
    return returnMaskedError();
  }

  return null;
};
