import {
  getAdhocEventsLogsRoute,
  getAdhocPendingEventsRoute,
  getAdhocProcessedEventsRoute,
  getAddAdhocEventRoute,
} from '../../../Common/utils/routesUtils';

export type AdhocEventsTab = 'add' | 'pending' | 'processed' | 'logs';
type TabInfo = {
  display_text: string;
  getRoute: () => string;
};

const tabInfo: Record<AdhocEventsTab, TabInfo> = {
  add: {
    display_text: 'Schedule an event',
    getRoute: () => getAddAdhocEventRoute('absolute'),
  },
  pending: {
    display_text: 'Pending events',
    getRoute: () => getAdhocPendingEventsRoute('absolute'),
  },
  processed: {
    display_text: 'Processed events',
    getRoute: () => getAdhocProcessedEventsRoute('absolute'),
  },
  logs: {
    display_text: 'Invocation logs',
    getRoute: () => getAdhocEventsLogsRoute('absolute'),
  },
};

export default tabInfo;
