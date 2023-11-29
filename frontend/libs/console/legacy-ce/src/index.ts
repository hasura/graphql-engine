import CommonScss from './lib/components/Common/Common.module.scss';
import filterQueryScss from './lib/components/Common/FilterQuery/FilterQuery.module.scss';
import tableScss from './lib/components/Common/TableCommon/Table.module.scss';

import DragFoldTable from './lib/components/Common/TableCommon/DragFoldTable';

import Editor from './lib/components/Common/Layout/ExpandableEditor/Editor';
import SearchableSelectBox from './lib/components/Common/SearchableSelect/SearchableSelect';

import { isMetadataStatusPage } from './lib/components/Error/ErrorBoundary';
import { redirectToMetadataStatus } from './lib/components/Common/utils/routesUtils';

import { ApiLimits } from './lib/components/Services/ApiExplorer/Security';
import { IntrospectionOptions } from './lib/components/Services/ApiExplorer/Security/Introspection';

import * as EndpointNamedExps from './lib/Endpoints';
import * as ControlPlane from './lib/features/ControlPlane';

export * from './lib/utils/console-dev-tools';

export { ControlPlane };

export { App as ConsoleCeApp } from './lib/client';

export type {
  Metadata,
  SetOpenTelemetryQuery,
  unexistingEnvVarSchema,
  hasuraEnvVarsNotAllowedSchema,
} from './lib/features/hasura-metadata-types';
export type { ServerConfig } from './lib/hooks';
export type {
  MetadataResponse,
  SchemaResponse,
} from './lib/features/MetadataAPI';
export { DragFoldTable };

export { Editor, SearchableSelectBox };
export { dataRouterUtils } from './lib/components/Services/Data/';
export { default as getActionsRouter } from './lib/components/Services/Actions/Router';
export { eventsRoutes } from './lib/components/Services/Events';
export { default as generatedApiExplorer } from './lib/components/Services/ApiExplorer/ApiExplorer';
export { default as generatedVoyagerConnector } from './lib/components/Services/VoyagerView/VoyagerView';
export { getRemoteSchemaRouter } from './lib/components/Services/RemoteSchema';
export { dataReducer } from './lib/components/Services/Data';
export { default as actionsReducer } from './lib/components/Services/Actions/reducer';
export { default as typesReducer } from './lib/components/Services/Types/reducer';
export { eventsReducer } from './lib/components/Services/Events';
export { default as apiExplorerReducer } from './lib/components/Services/ApiExplorer/Actions';
export { default as telemetryReducer } from './lib/telemetry/Actions';
export { default as invokeEventTriggerReducer } from './lib/components/Services/Events/EventTriggers/InvokeManualTrigger/InvokeManualTriggerAction';
export { remoteSchemaReducer } from './lib/components/Services/RemoteSchema';
export { modalReducer } from './lib/store/modal/modal.reducer';

export { metadataReducer } from './lib/metadata/reducer';
export { HasuraMetadataV3 } from './lib/metadata/types';
export { default as mainReducer } from './lib/components/Main/Actions';
export { default as notificationsReducer } from './lib/components/Services/Common/notifications.reducer';

export {
  persistGraphiQLHeaders,
  getPersistedGraphiQLHeaders,
} from './lib/components/Services/ApiExplorer/ApiRequest/utils';
export { fetchConsoleNotifications } from './lib/components/Main/Actions';
export { default as NotificationSection } from './lib/components/Main/NotificationSection';
export { default as Onboarding } from './lib/components/Common/Onboarding';
export {
  Analytics,
  startTracing,
  addUserProperties,
  programmaticallyTraceError,
  REDACT_EVERYTHING,
  InitializeTelemetry,
} from './lib/features/Analytics';
export { CloudOnboarding } from './lib/features/CloudOnboarding';
export { prefetchSurveysData } from './lib/features/Surveys';
export { prefetchOnboardingData } from './lib/features/CloudOnboarding/NeonOnboardingWizard';
export {
  prefetchEELicenseInfo,
  NavbarButton as EntepriseNavbarButton,
  WithEELiteAccess,
  useEELiteAccess,
} from './lib/features/EETrial';
export { default as PageNotFound } from './lib/components/Error/PageNotFound';
export * from './lib/new-components/Button/';
export * from './lib/new-components/Tooltip/';
export * from './lib/new-components/Badge/';
export * from './lib/new-components/Dialog';
export * from './lib/new-components/Toasts';
export { default as dataHeaders } from './lib/components/Services/Data/Common/Headers';
export { handleMigrationErrors } from './lib/components/Services/Data/TableModify/ModifyActions';
export { loadMigrationStatus } from './lib/components/Main/Actions';
export {
  fetchSchemaList,
  updateSchemaInfo,
  UPDATE_CURRENT_SCHEMA,
  UPDATE_DATA_HEADERS,
  ADMIN_SECRET_ERROR,
} from './lib/components/Services/Data/DataActions';
export { default as Spinner } from './lib/components/Common/Spinner/Spinner';
export { CommonScss };
export * from './lib/components/Services/Settings';
export {
  SchemaRegistryContainer,
  SchemaDetailsView,
} from './lib/features/SchemaRegistry';
export {
  loadInconsistentObjects,
  exportMetadata,
} from './lib/metadata/actions';
export { default as globals } from './lib/Globals';
export { default as endpoints } from './lib/Endpoints';
export { default as mainState } from './lib/components/Main/State';
export {
  changeRequestHeader,
  removeRequestHeader,
} from './lib/components/Services/ApiExplorer/Actions';
export { filterQueryScss, tableScss };
export * from './lib/components/Common';
export { loadConsoleOpts } from './lib/telemetry/Actions';
export * from './lib/telemetry';
export { default as Endpoints } from './lib/Endpoints';
export { EndpointNamedExps };

export { updateRequestHeaders } from './lib/components/Main/Main';

export {
  showErrorNotification,
  showSuccessNotification,
} from './lib/components/Services/Common/Notification';

export { default as CreateRestView } from './lib/components/Services/ApiExplorer/Rest/Form/';
export { default as RestListView } from './lib/components/Services/ApiExplorer/Rest/List';
export { default as DetailsView } from './lib/components/Services/ApiExplorer/Rest/Details';
export { default as ApiContainer } from './lib/components/Services/ApiExplorer/Container';

export {
  redirectToMetadataStatus,
  isMetadataStatusPage,
  ApiLimits,
  IntrospectionOptions,
};

export { ReactQueryProvider, reactQueryClient } from './lib/lib/reactQuery';

export { PrometheusSettings } from './lib/features/Prometheus';
export { QueryResponseCaching } from './lib/features/QueryResponseCaching';
export { MultipleAdminSecretsPage } from './lib/features/EETrial';
export { MultipleJWTSecretsPage } from './lib/features/EETrial';
export { SingleSignOnPage } from './lib/features/EETrial';

export { OpenTelemetryFeature } from './lib/features/OpenTelemetry';

export { FeatureFlags } from './lib/features/FeatureFlags';

export { isFeatureFlagEnabled } from './lib/features/FeatureFlags/hooks/useFeatureFlags';
export { availableFeatureFlagIds } from './lib/features/FeatureFlags';

export {
  isMonitoringTabSupportedEnvironment,
  isEnvironmentSupportMultiTenantConnectionPooling,
  isEEClassicConsole,
} from './lib/utils/proConsole';

export { isCloudConsole } from './lib/utils/cloudConsole';

export { AllowListDetail } from './lib/components/Services/AllowList/AllowListDetail';

export { default as generatedAdminSecretLoginConnector } from './lib/components/Login/Login';
export * from './lib/constants';
export * from './lib/components/AppState';

export * from './lib/components/App/Actions';
export { default as progressBarReducer } from './lib/components/App/Actions';

export {
  LS_KEYS,
  setLSItem,
  getLSItem,
  removeLSItem,
} from './lib/utils/localStorage';

export * from './lib/features/RestEndpoints';

export { reduxStoreListener } from './lib/store/utils/';

export { default as App } from './lib/components/App/App';
