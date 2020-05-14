import React from 'react';
import LeftSubSidebar from '../../../Common/Layout/LeftSubSidebar/LeftSubSidebar';
import getLeftSidebarSection from '../../../Common/Layout/LeftSubSidebar/LeftSidebarSection';
import { ScheduledTrigger, EventTrigger, EventKind } from '../types';
import { getSubserviceHeadings } from '../constants';
import {
  getAddETRoute,
  getAddSTRoute,
  getSTModifyRoute,
  getETModifyRoute,
} from '../../../Common/utils/routesUtils';

interface Props extends React.ComponentProps<'div'> {
  triggers: ScheduledTrigger[] | EventTrigger[];
  currentTrigger?: ScheduledTrigger | EventTrigger;
  service: Exclude<EventKind, 'scheduled'>;
}

const LeftSidebar: React.FC<Props> = props => {
  const { triggers, currentTrigger, service } = props;
  const {
    getChildList: getTriggersList,
    getSearchInput,
    count,
  } = getLeftSidebarSection({
    getServiceEntityLink: entityName => {
      switch (service) {
        case 'data':
          return getETModifyRoute(entityName);
          break;
        case 'cron':
          return getSTModifyRoute(entityName);
          break;
        default:
          return getSTModifyRoute(entityName);
          break;
      }
    },
    items: triggers,
    currentItem: currentTrigger,
    service,
  });

  // TODO, move to common utils
  const heading = `${getSubserviceHeadings(service).triggerHeading}s`;

  const isCronTrigger = service === 'cron';
  const addLink = isCronTrigger ? getAddSTRoute() : getAddETRoute();

  return (
    <LeftSubSidebar
      showAddBtn
      searchInput={getSearchInput()}
      heading={`${heading} (${count})`}
      addLink={addLink}
      addLabel="Create"
      addTestString={`${service}-sidebar-add`}
      childListTestString={`${service}-links`}
    >
      {getTriggersList()}
    </LeftSubSidebar>
  );
};

export default LeftSidebar;
