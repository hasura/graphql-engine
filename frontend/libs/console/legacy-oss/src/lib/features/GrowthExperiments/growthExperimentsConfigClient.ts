import globals from '@/Globals';
import { cloudDataServiceApiClient } from '@/hooks/cloudDataServiceApiClient';
import { ExperimentConfig, ExperimentsResponseData } from './types';
import { query } from './constants';
import { transformFn } from './utils';

export type GrowthExperimentsClient = {
  setAllExperimentConfig: () => Promise<void>;
  getAllExperimentConfig: () => ExperimentConfig[];
};

/**
 * Fetch all growth experiments data for the given user. Not making it a hook as this could be required in legacy class components.
 * Hence `setAllExperimentConfig` needs to be passed the correct headers from calling component. If caller is hook, use `useAppSelector`
 * to get the headers. Class based caller needs to subscribe to the global store to get headers.
 */
export function makeGrowthExperimentsClient(): GrowthExperimentsClient {
  /**
   * Experiments config data, transformed in required format.
   */
  let allExperimentsConfig: ExperimentConfig[] = [];

  /**
   * Makes variable `allExperimentsConfig` consistent with the server state. Call this in top level of your app to set
   * `allExperimentsConfig`. Also, when a server state mutation is done, use this function to keep data in sync.
   */

  const setAllExperimentConfig = () => {
    /*
     * Gracefully exit if current context is not cloud-console
     * and current user is not project owner
     */
    if (globals.consoleType !== 'cloud' || globals.userRole !== 'owner') {
      return Promise.resolve();
    }

    // cloud uses cookie-based auth, so does not require an admin secret
    const headers = {
      'content-type': 'application/json',
    };

    return cloudDataServiceApiClient<
      ExperimentsResponseData,
      ExperimentConfig[]
    >(query, {}, headers, transformFn)
      .then(res => {
        allExperimentsConfig = res;
        return Promise.resolve();
      })
      .catch(error => {
        // Possible enhancement: Add a retry mechanism
        console.error(error);
        return Promise.reject(error);
      });
  };

  /**
   * Returns latest `allExperimentsConfig` data. Note that it doesn't subscribe the calling component to changes in `allExperimentsConfig`.
   * Update the component UI manually if you're also doing a server state mutation using `setAllExperimentConfig`.
   */
  const getAllExperimentConfig = () => {
    return allExperimentsConfig;
  };

  return { getAllExperimentConfig, setAllExperimentConfig };
}
