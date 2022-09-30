const CommonScss = require('../lib/components/Common/Common.module.scss');
const filterQueryScss = require('../lib/components/Common/FilterQuery/FilterQuery.module.scss');
const tableScss = require('../lib/components/Common/TableCommon/Table.module.scss');

import * as EndpointNamedExps from '../lib/Endpoints';

export {
  persistGraphiQLHeaders,
  getPersistedGraphiQLHeaders,
} from '../lib/components/Services/ApiExplorer/ApiRequest/utils';
export { fetchConsoleNotifications } from '../lib/components/Main/Actions';
export { default as NotificationSection } from '../lib/components/Main/NotificationSection';
export { default as Onboarding } from '../lib/components/Common/Onboarding';
export { tracingTools } from '../lib/features/TracingTools';
export { OnboardingWizard } from '../lib/features/OnboardingWizard';
export { prefetchSurveysData } from '../lib/features/Surveys';
export { makeGrowthExperimentsClient } from '../lib/features/GrowthExperiments';
export { default as PageNotFound } from '../lib/components/Error/PageNotFound';
export * from '../lib/new-components/Button/';
export * from '../lib/new-components/Tooltip/';
export { CONSOLE_ADMIN_SECRET } from '../lib/components/AppState';
export { default as dataHeaders } from '../lib/components/Services/Data/Common/Headers';
export { handleMigrationErrors } from '../lib/components/Services/Data/TableModify/ModifyActions';
export { loadMigrationStatus } from '../lib/components/Main/Actions';
export {
  fetchSchemaList,
  updateSchemaInfo,
  UPDATE_CURRENT_SCHEMA,
  UPDATE_DATA_HEADERS,
  ADMIN_SECRET_ERROR,
} from '../lib/components/Services/Data/DataActions';
export { default as generatedVoyagerConnector } from '../lib/components/Services/VoyagerView/VoyagerView';
export { default as Spinner } from '../lib/components/Common/Spinner/Spinner';
export { CommonScss };
export * from '../lib/components/Services/Settings';
export {
  loadInconsistentObjects,
  exportMetadata,
} from '../lib/metadata/actions';
import { isMetadataStatusPage } from '../lib/components/Error/ErrorBoundary.tsx';
import { redirectToMetadataStatus } from '../lib/components/Common/utils/routesUtils.ts';
import { ApiLimits } from '../lib/components/Services/ApiExplorer/Security';
import { IntrospectionOptions } from '../lib/components/Services/ApiExplorer/Security/Introspection';

export { default as globals } from '../lib/Globals';
export { default as endpoints } from '../lib/Endpoints';
export { default as mainState } from '../lib/components/Main/State';
export {
  changeRequestHeader,
  removeRequestHeader,
} from '../lib/components/Services/ApiExplorer/Actions';
export { filterQueryScss, tableScss };
export * from '../lib/components/Common';
export { loadConsoleOpts } from '../lib/telemetry/Actions';
export * from '../lib/telemetry';
export { default as Endpoints } from '../lib/Endpoints';
export { EndpointNamedExps };

export { updateRequestHeaders } from '../lib/components/Main/Main';

export {
  showErrorNotification,
  showSuccessNotification,
} from '../lib/components/Services/Common/Notification';

export { default as CreateRestView } from '../lib/components/Services/ApiExplorer/Rest/Form/';
export { default as RestListView } from '../lib/components/Services/ApiExplorer/Rest/List';
export { default as DetailsView } from '../lib/components/Services/ApiExplorer/Rest/Details';
export { default as ApiContainer } from '../lib/components/Services/ApiExplorer/Container';

export {
  redirectToMetadataStatus,
  isMetadataStatusPage,
  ApiLimits,
  IntrospectionOptions,
};

export * from './table';
export { ReactQueryProvider, reactQueryClient } from '../lib/lib/reactQuery';

export { FeatureFlags } from '../lib/features/FeatureFlags';

export { isMonitoringTabSupportedEnvironment } from '../lib/utils/proConsole';

export {
  SampleDBBanner,
  newSampleDBTrial,
} from '../lib/components/Services/Data/DataSources/SampleDatabase';

export { AllowListDetail } from '../lib/components/Services/AllowList/AllowListDetail';
