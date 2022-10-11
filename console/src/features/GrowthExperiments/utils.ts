import { ExperimentConfig, ExperimentsResponseData } from './types';

/**
 * Transforms server returned data to the required format.
 */
export function transformFn(data: ExperimentsResponseData): ExperimentConfig[] {
  return data.data.experiments_config.map(experimentConfig => {
    const experimentCohort = data.data.experiments_cohort.find(
      cohort => cohort.experiment === experimentConfig.experiment
    );
    const userActivity = experimentCohort?.activity ?? {};
    return {
      experiment: experimentConfig.experiment,
      status: experimentConfig.status,
      metadata: experimentConfig.metadata,
      userActivity,
    };
  });
}
