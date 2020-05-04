import React from 'react';
import LeftSubSidebar from '../../../Common/Layout/LeftSubSidebar/LeftSubSidebar';
import getLeftSidebarSection from '../../../Common/Layout/LeftSubSidebar/LeftSidebarSection';
import { ScheduledTrigger, EventTrigger } from '../Types';
import {
  getAddETRoute,
  getAddSTRoute,
  getSTModifyRoute,
  getETModifyRoute,
} from '../../../Common/utils/routesUtils';

interface LeftSidebarProps extends React.ComponentProps<'div'> {
  triggers: ScheduledTrigger[] | EventTrigger[];
  currentTrigger?: ScheduledTrigger | EventTrigger;
  service: string;
}

const LeftSidebar: React.FC<LeftSidebarProps> = ({
  triggers,
  currentTrigger,
  service,
}) => {
  const {
    getChildList: getTriggersList,
    getSearchInput,
    count,
  } = getLeftSidebarSection({
    getServiceEntityLink: entityName => {
      if (service.includes('scheduled')) {
        return getSTModifyRoute(entityName);
      }
      return getETModifyRoute(entityName);
    },
    items: triggers,
    currentItem: currentTrigger,
    service,
  });

  // TODO, move to common utils
  const heading = service[0].toUpperCase() + service.substr(1, service.length);

  const isScheduledTrigger = service.includes('scheduled');
  const addLink = isScheduledTrigger ? getAddSTRoute() : getAddETRoute();

  return (
    <LeftSubSidebar
      showAddBtn
      searchInput={getSearchInput()}
      heading={`${heading} (${count})`}
      addLink={addLink}
      addLabel="Create"
      addTestString={`${isScheduledTrigger ? 'st' : 'et'}-sidebar-add`}
      childListTestString={`${isScheduledTrigger ? 'st' : 'et'}-links`}
    >
      {getTriggersList()}
    </LeftSubSidebar>
  );
};

export default LeftSidebar;
