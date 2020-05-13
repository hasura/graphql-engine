import {
  getSTModifyRoute,
  getSTInvocationLogsRoute,
  getSTPendingEventsRoute,
  getSTProcessedEventsRoute,
} from '../../../Common/utils/routesUtils';

export type STTab = 'modify' | 'pending' | 'processed' | 'logs';
type TabInfo = {
  display_text: string;
  getRoute: (triggerName: string) => string;
};

const tabInfo: Record<STTab, TabInfo> = {
  modify: {
    display_text: 'Modify',
    getRoute: (triggerName: string) => getSTModifyRoute(triggerName),
  },
  pending: {
    display_text: 'Pending events',
    getRoute: (triggerName: string) => getSTPendingEventsRoute(triggerName),
  },
  processed: {
    display_text: 'Processed events',
    getRoute: (triggerName: string) => getSTProcessedEventsRoute(triggerName),
  },
  logs: {
    display_text: 'Invocation logs',
    getRoute: (triggerName: string) => getSTInvocationLogsRoute(triggerName),
  },
};

export default tabInfo;
