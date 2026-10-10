import { dataRoutes } from '@hasura/shared/utils';

export const appPrefix = '/events';

export type AdhocEventsTab = 'add' | 'pending' | 'processed' | 'logs' | 'info';

type TabInfo = {
  display_text: string;
  getRoute: () => string;
};

export const tabInfo: Record<AdhocEventsTab, TabInfo> = {
  info: {
    display_text: 'Info',
    getRoute: () => dataRoutes.getAdhocEventsInfoRoute('absolute'),
  },
  add: {
    display_text: 'Schedule an event',
    getRoute: () => dataRoutes.getAddAdhocEventRoute('absolute'),
  },
  pending: {
    display_text: 'Pending events',
    getRoute: () => dataRoutes.getAdhocPendingEventsRoute('absolute'),
  },
  processed: {
    display_text: 'Processed events',
    getRoute: () => dataRoutes.getAdhocProcessedEventsRoute('absolute'),
  },
  logs: {
    display_text: 'Invocation logs',
    getRoute: () => dataRoutes.getAdhocEventsLogsRoute('absolute'),
  },
};
