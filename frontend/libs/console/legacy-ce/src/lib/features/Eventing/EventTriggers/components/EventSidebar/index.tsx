import React from 'react';
import { FaTable } from 'react-icons/fa';
import { dataRoutes } from '@hasura/shared/utils';
import { TreeView } from '../../../../../components/Common/Layout/LeftSubSidebar/TreeView';
import { useMetadata } from '@hasura/metadata/api';
import { DATA_EVENTS_HEADING } from '@hasura/shared/types';
import { LeftSubSidebar, useLeftSidebarSection } from '@hasura/shared/ui';

interface Props {
  triggerName: string | undefined;
}

const EventSidebar: React.FC<Props> = ({ triggerName }) => {
  const { data: triggers, isFetching } = useMetadata((m) =>
    m.metadata.sources.flatMap((source) =>
      source.tables
        .flatMap((t) => t.event_triggers ?? [])
        .map((et) => ({
          source: source.name,
          name: et.name,
        }))
        .sort((a, b) =>
          a.name.toLowerCase().localeCompare(b.name.toLowerCase()),
        ),
    ),
  );
  const getEntityLink = (entityName: string) => {
    const encodedEntityName = encodeURIComponent(entityName);
    return dataRoutes.getETModifyRoute({ name: encodedEntityName });
  };

  const sidebarIcon = <FaTable aria-hidden="true" />;

  const currentTrigger = triggerName
    ? triggers?.find((et) => et.name === triggerName)
    : undefined;

  const { getSearchInput, count, items, searchText } = useLeftSidebarSection({
    getServiceEntityLink: getEntityLink,
    items: triggers ?? [],
    currentItem: currentTrigger,
    service: 'triggers',
    sidebarIcon,
  });

  const heading = DATA_EVENTS_HEADING;
  const addLink = dataRoutes.getAddETRoute();

  return (
    <LeftSubSidebar
      loading={isFetching}
      showAddBtn
      searchInput={getSearchInput()}
      heading={`${heading} (${count})`}
      addLink={addLink}
      addLabel="Create"
      addTestString={`event-sidebar-add`}
      childListTestString={`event-links`}
    >
      <TreeView
        items={items}
        icon={sidebarIcon}
        service="triggers"
        currentItem={currentTrigger}
        getServiceEntityLink={getEntityLink}
        searchText={searchText}
      />
    </LeftSubSidebar>
  );
};

export default EventSidebar;
