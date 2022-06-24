export type OnboardingSampleDBCohortConfig = {
  databaseUrl: string;
  status: 'enabled' | 'disabled' | 'concluded';
};

export type StateType = {
  cohortConfig: OnboardingSampleDBCohortConfig | null;
};

export const initState: StateType = {
  cohortConfig: null,
};
