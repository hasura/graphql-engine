import { useLocation } from 'react-router';
import { IoGitBranch } from 'react-icons/io5';
import {
  WarningSymbol,
  LeftSubSidebar,
  LeftSidebarLeafNavItem,
  Text,
  Input,
} from '@hasura/shared/ui';
import { useAppContext } from '@hasura/shared/context';
import { useInconsistentMetadata, useMetadata } from '@hasura/metadata/api';
import { appPrefix } from '../../constants';
import { useMemo, useState } from 'react';
import { Em, Flex } from '@radix-ui/themes';

const RemoteSchemaSubSidebar = () => {
  const location = useLocation();
  const { readOnlyMode } = useAppContext();
  const { data: meta } = useMetadata();
  const { data: inconsistentRemoteSchemas } = useInconsistentMetadata(
    (metadata) =>
      metadata?.inconsistent_objects?.filter(
        (inconObject) =>
          'type' in inconObject && inconObject.type === 'remote_schema',
      ) ?? [],
  );

  const [searchQuery, setSearchQuery] = useState('');

  const filteredData = useMemo(() => {
    const query = searchQuery.trim().toLowerCase();
    const matchedSchemas = query
      ? meta?.metadata.remote_schemas?.filter((rm) =>
          rm.name.toLowerCase().includes(query),
        )
      : meta?.metadata.remote_schemas;

    return matchedSchemas ?? [];
  }, [meta, searchQuery]);

  const getChildList = () => {
    if (filteredData.length === 0) {
      return (
        <Text as="p" data-test="remote-schema-sidebar-no-schemas">
          <Em>No remote schemas available</Em>
        </Text>
      );
    }

    return filteredData.map((d, i) => {
      const isActive =
        location.pathname.includes(`${appPrefix}/`) &&
        location.pathname.includes(`/${d.name}/`);

      const inconsistentCurrentSchema = inconsistentRemoteSchemas?.find(
        (elem) => elem.definition?.name === d.name,
      );

      return (
        <LeftSidebarLeafNavItem
          key={i}
          to={`${appPrefix}/manage/${encodeURIComponent(d.name)}/details`}
          isActive={isActive}
        >
          <Flex align="center" gap="1">
            <IoGitBranch aria-hidden="true" size="12px" />
            {d.name}
            {inconsistentCurrentSchema ? (
              <WarningSymbol
                customStyle="ml-xs"
                tooltipText={
                  'This remote schema is in an inconsistent state. ' +
                  'Fields from this remote schema are currently not exposed over the GraphQL API'
                }
                tooltipPlacement="right"
              />
            ) : null}
          </Flex>
        </LeftSidebarLeafNavItem>
      );
    });
  };

  const remoteSchemaCount = meta?.metadata.remote_schemas?.length ?? 0;

  return (
    <LeftSubSidebar
      showAddBtn={!readOnlyMode}
      searchInput={
        <Input
          type="text"
          onChange={(e) => setSearchQuery(e.target.value)}
          placeholder="search remote schemas"
          data-test="search-remote-schemas"
        />
      }
      heading={`Remote Schemas (${remoteSchemaCount})`}
      addLink={`${appPrefix}/manage/add`}
      addLabel={'Add'}
      addTestString={'remote-schema-sidebar-add-table'}
      childListTestString={'remote-schema-table-links'}
    >
      {getChildList()}
    </LeftSubSidebar>
  );
};

export default RemoteSchemaSubSidebar;
