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
import { TreeView } from '../../Common/Layout/LeftSubSidebar/TreeView';

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
        return 'fa-table';
      default:
        return 'fa-wrench';
    }
  };

  const getEntityLink = (entityName: string) => {
    const encodedEntityName = encodeURIComponent(entityName);
    switch (service) {
      case 'data':
        return getETModifyRoute({ name: encodedEntityName });
      case 'cron':
        return getSTModifyRoute(encodedEntityName);
      default:
        return getSTModifyRoute(encodedEntityName);
    }
  };

  const { getSearchInput, count, items, getChildList } = getLeftSidebarSection({
    getServiceEntityLink: getEntityLink,
    items: triggers,
    currentItem: currentTrigger,
    service: 'triggers',
    sidebarIcon: getSidebarIcon(),
  });

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
      {service === 'data' ? (
        <TreeView
          items={items as EventTrigger[]}
          icon={getSidebarIcon()}
          service="triggers"
          currentItem={currentTrigger as EventTrigger}
          getServiceEntityLink={getEntityLink}
        />
      ) : (
        getChildList()
      )}
    </LeftSubSidebar>
  );
};

export default LeftSidebar;
