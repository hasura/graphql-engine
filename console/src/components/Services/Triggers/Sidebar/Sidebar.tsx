import React from 'react';
import LeftSubSidebar from '../../../Common/Layout/LeftSubSidebar/LeftSubSidebar';
import getLeftSidebarSection from '../../../Common/Layout/LeftSubSidebar/LeftSidebarSection';
import { ScheduledTrigger, EventTrigger } from '../Types';

interface LeftSidebarProps extends React.ComponentProps<'div'> {
  appPrefix: string;
  triggers: ScheduledTrigger[] | EventTrigger[];
  currentTrigger: ScheduledTrigger | EventTrigger;
  service: string;
}

const LeftSidebar: React.FC<LeftSidebarProps> = ({
  appPrefix,
  triggers,
  currentTrigger,
  service,
}) => {
  const {
    getChildList: getTriggersList,
    getSearchInput,
    count,
  } = getLeftSidebarSection({
    appPrefix,
    items: triggers,
    currentItem: currentTrigger,
    service,
  });

  // TODO, move to common utils
  const heading = service[0].toUpperCase() + service.substr(1, service.length);

  const isScheduledTrigger = service.includes('scheduled');
  const addLink = `${appPrefix}/${
    isScheduledTrigger ? 'scheduled' : 'events'
  }/add`;

  return (
    <LeftSubSidebar
      showAddBtn
      searchInput={getSearchInput()}
      heading={`${heading} (${count})`}
      addLink={addLink}
      addLabel={'Create'}
      addTestString={`${isScheduledTrigger ? 'st' : 'et'}-sidebar-add-table`}
      childListTestString={`${isScheduledTrigger ? 'st' : 'et'}-links`}
    >
      {getTriggersList()}
    </LeftSubSidebar>
  );
};

export default LeftSidebar;
