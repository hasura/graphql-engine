import React from 'react';
import { useGetAllCronTriggers } from '../hooks/useGetAllCronTriggers';
import { dataRoutes } from '@hasura/shared/utils';
import { FaCalendarAlt } from 'react-icons/fa';
import { CRON_EVENTS_HEADING } from '@hasura/shared/types';
import {
  IndicatorCard,
  LeftSubSidebar,
  useLeftSidebarSection,
} from '@hasura/shared/ui';
import { Flex, Skeleton } from '@radix-ui/themes';

interface Props {
  triggerName: string | undefined;
}

const CronTriggerSidebar: React.FC<Props> = ({ triggerName }) => {
  const {
    data: triggers,
    isLoading,
    error,
  } = useGetAllCronTriggers({
    select: (result) =>
      result.map((value) => ({
        name: value.name,
      })),
  });

  const { getSearchInput, count, getChildList } = useLeftSidebarSection({
    getServiceEntityLink: (entityName: string) => {
      const encodedEntityName = encodeURIComponent(entityName);
      return dataRoutes.getSTModifyRoute(encodedEntityName);
    },
    items: triggers ?? [],
    currentItem: triggerName ? { name: triggerName } : undefined,
    service: 'triggers',
    sidebarIcon: <FaCalendarAlt aria-hidden="true" className="mr-2" />,
  });

  if (isLoading) {
    return <Skeleton height="20px" />;
  }

  if (!triggers?.length && !isLoading) {
    return (
      <IndicatorCard status="negative">
        Could not find any cron triggers
      </IndicatorCard>
    );
  }

  if (error) {
    return (
      <IndicatorCard status="negative">
        There was an error, please try again later
      </IndicatorCard>
    );
  }

  const heading = CRON_EVENTS_HEADING;
  const addLink = dataRoutes.getAddSTRoute();

  return (
    <LeftSubSidebar
      showAddBtn
      searchInput={getSearchInput()}
      heading={`${heading} (${count})`}
      addLink={addLink}
      addLabel="Create"
      addTestString={`cron-sidebar-add`}
      childListTestString={`cron-links`}
    >
      <Flex direction="column" gap="2">
        {getChildList()}
      </Flex>
    </LeftSubSidebar>
  );
};

export default CronTriggerSidebar;
