import React from 'react';
import LeftSubSidebar from '../../Common/Layout/LeftSubSidebar/LeftSubSidebar';
import getLeftSidebarSection from '../../Common/Layout/LeftSubSidebar/LeftSidebarSection';
import { ScheduledTrigger, EventTrigger, EventKind } from './types';
import { getSubserviceHeadings } from './constants';
import {
  getAddETRoute,
  getAddSTRoute,
  getSTModifyRoute,
  getETModifyRoute,
} from '../../Common/utils/routesUtils';

interface Props {
  triggers: ScheduledTrigger[] | EventTrigger[];
  currentTrigger?: ScheduledTrigger | EventTrigger;
  service: Exclude<EventKind, 'scheduled'>;
}

const LeftSidebar: React.FC<Props> = props => {
  const { triggers, currentTrigger, service } = props;

  const getSidebarIcon = () => {
    switch (service) {
      case 'cron':
        return 'fa-calendar';
      case 'data':
        return 'fa-database';
      default:
        return 'fa-wrench';
    }
  };

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
    service: 'triggers',
    sidebarIcon: getSidebarIcon(),
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
