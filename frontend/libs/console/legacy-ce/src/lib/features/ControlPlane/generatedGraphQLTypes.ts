/** Internal type. DO NOT USE DIRECTLY. */
type Exact<T extends { [key: string]: unknown }> = { [K in keyof T]: T[K] };
/** Internal type. DO NOT USE DIRECTLY. */
export type Incremental<T> =
  | T
  | {
      [P in keyof T]?: P extends ' $fragmentName' | '__typename' ? T[P] : never;
    };
export type SurveyResponseV2 = {
  additionalInfo?: string | null | undefined;
  answer?: string | null | undefined;
  optionSelected?: string | null | undefined;
  questionId: string;
};

export type UpdateEnvObject = {
  key: string;
  value: string;
};

export enum Experiments_Enum {
  /** Console Onboarding Wizard v1 for new users */
  ConsoleOnboardingWizardV1 = 'console_onboarding_wizard_v1',
}

export enum One_Click_Deployment_States_Enum {
  /** Applying metadata, migration and seed data to the project */
  ApplyingMetadataMigrationsSeeds = 'APPLYING_METADATA_MIGRATIONS_SEEDS',
  /** Waiting for environment variables from user to be available */
  AwaitingEnvironmentVariables = 'AWAITING_ENVIRONMENT_VARIABLES',
  /** Cloning git repository */
  CloningGitRepository = 'CLONING_GIT_REPOSITORY',
  /** One click deployment executed successfully */
  Completed = 'COMPLETED',
  /** Some error occurred while deploying the git repository to hasura cloud project */
  Error = 'ERROR',
  /** One click deployment initiated */
  Initialized = 'INITIALIZED',
  /** Checking if the required environment variables are present in the project */
  ReadingEnvironmentVariables = 'READING_ENVIRONMENT_VARIABLES',
  /** No environment variables required from the user */
  SufficientEnvironmentVariables = 'SUFFICIENT_ENVIRONMENT_VARIABLES',
}

export enum Project_Entitlement_Types_Enum {
  /** Azure Monitor APM integration: export a project's logs and metrics to Azure Monitor. */
  ApmIntegrationAzuremonitor = 'apm_integration_azuremonitor',
  /** Datadog APM integration: export a project's logs and metrics to Datadog. */
  ApmIntegrationDatadog = 'apm_integration_datadog',
  /** New Relic APM integration: export a project's logs and metrics to New Relic. */
  ApmIntegrationNewrelic = 'apm_integration_newrelic',
  /** Opentelemetry APM integration: export a project's logs and metrics to Opentelemetry. */
  ApmIntegrationOpentelemetry = 'apm_integration_opentelemetry',
  /** Prometheus APM integration: provides an endpoint to fetch project's metrics for ingestion into Prometheus server. */
  ApmIntegrationPrometheus = 'apm_integration_prometheus',
  /** Maximum number of collaborators that can be added to a project */
  CollaboratorLimit = 'collaborator_limit',
  /** Allow to add a collaborator to the project with admin privilege. */
  CollaboratorPrivilegeAdmin = 'collaborator_privilege_admin',
  /** Allow to add a collaborator to the project with graphql_admin privilege. */
  CollaboratorPrivilegeGraphqlAdmin = 'collaborator_privilege_graphql_admin',
  /** Allow to add a collaborator to the project with view_metrics privilege. */
  CollaboratorPrivilegeViewMetrics = 'collaborator_privilege_view_metrics',
  /** Configure access to the metrics tab on the project's HGE console. */
  ConsoleMetricsTab = 'console_metrics_tab',
  /** Configure custom domains for projects. */
  CustomDomainLimit = 'custom_domain_limit',
  /** Maximum amount of data passthrough allowed for a project per month. Unit is bytes. */
  DataPassthroughLimit = 'data_passthrough_limit',
  /** Maximum number of databases that can be connected to a project */
  DbLimit = 'db_limit',
  /** Configure Graphql allow lists for a project */
  GqlAllowLists = 'gql_allow_lists',
  /** Configure multiple admin secrets for a project */
  MultipleAdminSecrets = 'multiple_admin_secrets',
  /** Configure multiple JWT secrets for a project */
  MultipleJwt = 'multiple_jwt',
  /** Cost per hour if the project is not connected to any database. */
  NoDb = 'no_db',
  /** Cost and access to connecting a non Postgres databases to a project. */
  NonPgDb = 'non_pg_db',
  /** Configure access and limit for read replicas */
  ReadReplicas = 'read_replicas',
  /** Move a project between cloud host regions */
  RegionMigration = 'region_migration',
  /** Configure access to the metrics endpoint on tenant */
  ServerMetricsEndpoint = 'server_metrics_endpoint',
  /** Cost and access to connecting a Vanilla Postgres databases to a project. */
  VanillaPgDb = 'vanilla_pg_db',
}

export enum Survey_V2_Question_Kind_Enum {
  Checkbox = 'checkbox',
  Dropdown = 'dropdown',
  Radio = 'radio',
  /** 10 */
  Rating = 'rating',
  Text = 'text',
}

export type FetchAllExperimentsDataQueryVariables = Exact<{
  [key: string]: never;
}>;

export type FetchAllExperimentsDataQuery = {
  experiments_config: Array<{
    experiment: Experiments_Enum;
    metadata: any;
    status: string;
  }>;
  experiments_cohort: Array<{ experiment: Experiments_Enum; activity: any }>;
};

export type GetTenantEnvQueryVariables = Exact<{
  tenantId: string;
}>;

export type GetTenantEnvQuery = {
  getTenantEnv: { hash: string; envVars: any } | null;
};

export type TrackExperimentsCohortActivityMutationVariables = Exact<{
  projectId: string;
  experimentId: string;
  kind: string;
  error_code?: string | null | undefined;
}>;

export type TrackExperimentsCohortActivityMutation = {
  trackExperimentsCohortActivity: { status: string } | null;
};

export type UpdateTenantMutationVariables = Exact<{
  tenantId: string;
  currentHash: string;
  envs: Array<UpdateEnvObject> | UpdateEnvObject;
}>;

export type UpdateTenantMutation = {
  updateTenantEnv: { hash: string; envVars: any } | null;
};

export type NeonCreateDatabaseMutationVariables = Exact<{
  projectId: string;
}>;

export type NeonCreateDatabaseMutation = {
  neonCreateDatabase: {
    databaseUrl: string | null;
    email: string | null;
    envVar: string | null;
    isAuthenticated: boolean;
  } | null;
};

export type NeonTokenExchangeMutationVariables = Exact<{
  code: string;
  state: string;
  projectId: string;
}>;

export type NeonTokenExchangeMutation = {
  neonExchangeOAuthToken: { accessToken: string; email: string };
};

export type CheckDbLatencyMutationVariables = Exact<{
  project_id: string;
}>;

export type CheckDbLatencyMutation = {
  checkDBLatency: { db_latency_job_id: string } | null;
};

export type FetchInfoFromJobIdQueryVariables = Exact<{
  id: string;
}>;

export type FetchInfoFromJobIdQuery = {
  jobs_by_pk: {
    id: string;
    status: string;
    tasks: Array<{
      id: string;
      name: string;
      task_events: Array<{
        id: string;
        event_type: string;
        public_event_data: any;
        error: string | null;
      }>;
    }>;
  } | null;
};

export type InsertInfoIntoDbLatencyQueryMutationVariables = Exact<{
  jobId: string;
  projectId: string;
  isLatencyDisplayed: boolean;
  dateDifferenceInMilliseconds: number;
}>;

export type InsertInfoIntoDbLatencyQueryMutation = {
  insert_db_latency_one: { id: string } | null;
};

export type UpdateUserClickedChangeProjectRegionMutationVariables = Exact<{
  rowId: string;
  isChangeRegionClicked: boolean;
}>;

export type UpdateUserClickedChangeProjectRegionMutation = {
  update_db_latency: {
    affected_rows: number;
    returning: Array<{ id: string; is_change_region_clicked: boolean }>;
  } | null;
};

export type FetchOneClickDeploymentStateLogSubscriptionSubscriptionVariables =
  Exact<{
    id: string;
  }>;

export type FetchOneClickDeploymentStateLogSubscriptionSubscription = {
  one_click_deployment_by_pk: {
    id: string;
    one_click_deployment_state_logs: Array<{
      id: string;
      additional_info: any;
      from_state: One_Click_Deployment_States_Enum;
      to_state: One_Click_Deployment_States_Enum;
    }>;
  } | null;
};

export type TriggerOneClickDeploymentMutationVariables = Exact<{
  projectId: string;
}>;

export type TriggerOneClickDeploymentMutation = {
  triggerOneClickDeployment: { message: string | null; status: string } | null;
};

export type FetchAllSurveysDataQueryVariables = Exact<{
  currentTime: string;
}>;

export type FetchAllSurveysDataQuery = {
  survey_v2: Array<{
    survey_name: string;
    survey_title: string | null;
    survey_description: string | null;
    template_config: any;
    survey_questions: Array<{
      id: string;
      position: number;
      question: string;
      kind: Survey_V2_Question_Kind_Enum;
      is_mandatory: boolean;
      survey_question_options: Array<{
        id: string;
        position: number;
        option: string;
        template_config: any;
        additional_info_config: {
          info_description: string | null;
          is_mandatory: boolean;
        } | null;
      }>;
    }>;
    survey_responses: Array<{
      survey_response_answers: Array<{
        survey_question_id: string;
        survey_response_answer_options: Array<{
          answer: string | null;
          additional_info: string | null;
          option_id: string | null;
        }>;
      }>;
    }>;
  }>;
};

export type AddSurveyAnswerV2MutationVariables = Exact<{
  responses: Array<SurveyResponseV2 | null | undefined> | SurveyResponseV2;
  surveyName: string;
  projectID?: string | null | undefined;
}>;

export type AddSurveyAnswerV2Mutation = {
  saveSurveyAnswerV2: { status: string } | null;
};

export type AddSchemaRegistryFeatureRequestMutationVariables = Exact<{
  details: any;
}>;

export type AddSchemaRegistryFeatureRequestMutation = {
  addFeatureRequest: { status: string } | null;
};

export type FetchConfigStatusSubscriptionVariables = Exact<{
  tenantId: string;
}>;

export type FetchConfigStatusSubscription = {
  config_status: Array<{ hash: string; message: string | null }>;
};

export type FetchProjectInfoQueryVariables = Exact<{
  id: string;
}>;

export type FetchProjectInfoQuery = {
  users: Array<{ id: string }>;
  projects_by_pk: {
    id: string;
    plan_name: string | null;
    owner: { id: string | null } | null;
    collaborators: Array<{
      id: string;
      collaborator: { id: string | null } | null;
      project_collaborator_privileges: Array<{ privilege_slug: string }>;
    }>;
    tenant: { region_info: { metrics_fqdn: string | null } | null } | null;
    entitlements: Array<{
      id: string;
      entitlement: {
        type: Project_Entitlement_Types_Enum;
        config_is_enabled: boolean;
      };
    }>;
  } | null;
};
