const CommonScss = require('../src/components/Common/Common.scss');
const filterQueryScss = require('../src/components/Common/FilterQuery/FilterQuery.scss');
const tableScss = require('../src/components/Common/TableCommon/Table.scss');

import * as EndpointNamedExps from '../src/Endpoints';

export {
  persistGraphiQLHeaders,
  getPersistedGraphiQLHeaders,
} from '../src/components/Services/ApiExplorer/ApiRequest/utils';
export { fetchConsoleNotifications } from '../src/components/Main/Actions';
export { default as NotificationSection } from '../src/components/Main/NotificationSection';
export { default as Onboarding } from '../src/components/Common/Onboarding';
export { default as PageNotFound } from '../src/components/Error/PageNotFound';
export { CONSOLE_ADMIN_SECRET } from '../src/components/AppState';
export { default as dataHeaders } from '../src/components/Services/Data/Common/Headers';
export { handleMigrationErrors } from '../src/components/Services/Data/TableModify/ModifyActions';
export { loadMigrationStatus } from '../src/components/Main/Actions';
export {
  fetchSchemaList,
  updateSchemaInfo,
  UPDATE_CURRENT_SCHEMA,
  UPDATE_DATA_HEADERS,
  ADMIN_SECRET_ERROR,
} from '../src/components/Services/Data/DataActions';
export { default as generatedVoyagerConnector } from '../src/components/Services/VoyagerView/VoyagerView';
export { default as Spinner } from '../src/components/Common/Spinner/Spinner';
export { CommonScss };
export * from '../src/components/Services/Settings';
export {
  loadInconsistentObjects,
  exportMetadata,
} from '../src/metadata/actions';
import { isMetadataStatusPage } from '../src/components/Error/ErrorBoundary.tsx';
import { redirectToMetadataStatus } from '../src/components/Common/utils/routesUtils.ts';
import { ApiLimits } from '../src/components/Services/ApiExplorer/Security';
import { IntrospectionOptions } from '../src/components/Services/ApiExplorer/Security/Introspection';

export { default as globals } from '../src/Globals';
export { default as endpoints } from '../src/Endpoints';
export { default as mainState } from '../src/components/Main/State';
export {
  changeRequestHeader,
  removeRequestHeader,
} from '../src/components/Services/ApiExplorer/Actions';
export { filterQueryScss, tableScss };
export * from '../src/components/Common';
export { loadConsoleOpts } from '../src/telemetry/Actions';
export * from '../src/telemetry';
export { default as Endpoints } from '../src/Endpoints';
export { EndpointNamedExps };

export { updateRequestHeaders } from '../src/components/Main/Main';

export {
  showErrorNotification,
  showSuccessNotification,
} from '../src/components/Services/Common/Notification';

export { default as CreateRestView } from '../src/components/Services/ApiExplorer/Rest/Create/';
export { default as RestListView } from '../src/components/Services/ApiExplorer/Rest/List';
export { default as DetailsView } from '../src/components/Services/ApiExplorer/Rest/Details';
export { default as ApiContainer } from '../src/components/Services/ApiExplorer/Container';

export {
  redirectToMetadataStatus,
  isMetadataStatusPage,
  ApiLimits,
  IntrospectionOptions,
};

export * from './table';
