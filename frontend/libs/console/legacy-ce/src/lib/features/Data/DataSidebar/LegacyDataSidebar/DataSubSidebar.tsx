import {
  extractTableInfo,
  getTableDisplayName,
  dataRoutes,
} from '@hasura/shared/utils';
import { LeftSubSidebar, useLeftSidebarSection } from '@hasura/shared/ui';
import { useGDCTreeItemClick } from './GDCTree/hooks/useGDCTreeItemClick';
import { SourceItem } from './types';
import { Metadata } from '@hasura/shared/types';
import { adaptFunction, functionDisplayName } from '@hasura/metadata/helpers';
import { FaTable } from 'react-icons/fa6';
import TreeView from './TreeView';

type Props = {
  metadata: Metadata['metadata'] | undefined;
  metadataLoading: boolean;
};

const DataSubSidebar = ({ metadata, metadataLoading }: Props) => {
  const { handleClick } = useGDCTreeItemClick();
  const sources = metadata?.sources ?? [];

  const tableItems: SourceItem[] = sources.flatMap((source) =>
    source.tables
      .map<SourceItem>((t) => {
        const tableInfo = extractTableInfo(t.table);
        const displayName = getTableDisplayName(t.table);

        return {
          name: displayName,
          schema: tableInfo?.schema,
          tableName: tableInfo?.name ?? displayName,
          table: t.table,
          source: source.name,
          type: t.is_enum ? 'enum' : 'table',
        };
      })
      .concat(
        source.functions?.map<SourceItem>((fn) => {
          const functionInfo = adaptFunction(fn.function);
          const displayName = functionDisplayName({
            qualifiedFunction: fn.function,
          });

          return {
            name: displayName,
            schema: functionInfo?.schema,
            tableName: functionInfo?.name ?? displayName,
            table: fn.function,
            source: source.name,
            type: 'function',
          };
        }) ?? [],
      ),
  );

  const { getSearchInput, items } = useLeftSidebarSection({
    getServiceEntityLink: () => '',
    items: tableItems ?? [],
    service: 'tables',
    sidebarIcon: <FaTable aria-hidden="true" />,
  });

  return (
    <div>
      <LeftSubSidebar
        loading={metadataLoading}
        showAddBtn
        searchInput={getSearchInput()}
        heading={`Databases (${sources.length})`}
        addLink={dataRoutes.manageDatabasesRoute}
        addLabel="Manage"
        addTestString={`database-manage`}
        childListTestString={`database-links`}
      >
        <TreeView
          sources={sources}
          items={items}
          databaseLoading={metadataLoading}
          schemaLoading={metadataLoading}
          preLoadState={metadataLoading}
          gdcItemClick={handleClick}
        />
      </LeftSubSidebar>
    </div>
  );
};

export default DataSubSidebar;
