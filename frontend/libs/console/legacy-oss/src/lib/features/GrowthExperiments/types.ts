export type ExperimentConfig = {
  experiment: string;
  status: string;
  metadata: ExperimentsResponseData['data']['experiments_config'][0]['metadata'];
  userActivity: ExperimentsResponseData['data']['experiments_cohort'][0]['activity'];
};

export type ExperimentsResponseData = {
  data: {
    experiments_config: [
      {
        experiment: string;
        metadata: Record<string, unknown>;
        status: string;
      }
    ];
    experiments_cohort: [
      {
        experiment: string;
        activity: Record<string, unknown>;
      }
    ];
  };
};
