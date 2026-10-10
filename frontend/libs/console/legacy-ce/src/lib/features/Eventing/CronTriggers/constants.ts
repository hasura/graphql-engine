import { dataRoutes } from '@hasura/shared/utils';

export type STTab = 'modify' | 'pending' | 'processed' | 'logs';
type TabInfo = {
  display_text: string;
  getRoute: (triggerName: string) => string;
};

const tabInfo: Record<STTab, TabInfo> = {
  modify: {
    display_text: 'Modify',
    getRoute: (triggerName: string) => dataRoutes.getSTModifyRoute(triggerName),
  },
  pending: {
    display_text: 'Pending events',
    getRoute: (triggerName: string) =>
      dataRoutes.getSTPendingEventsRoute(triggerName),
  },
  processed: {
    display_text: 'Processed events',
    getRoute: (triggerName: string) =>
      dataRoutes.getSTProcessedEventsRoute(triggerName),
  },
  logs: {
    display_text: 'Invocation logs',
    getRoute: (triggerName: string) =>
      dataRoutes.getSTInvocationLogsRoute(triggerName),
  },
};

export default tabInfo;
