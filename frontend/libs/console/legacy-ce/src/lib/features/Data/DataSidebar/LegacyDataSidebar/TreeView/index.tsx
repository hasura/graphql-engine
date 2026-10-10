import React, { Key } from 'react';
import '../../../../../components/Common/Layout/LeftSubSidebar/LeftSubSidebar.module.scss';
import { SourceItem } from '../types';
import DatabaseItemsView from './DatabaseItemsView';
import groupBy from 'lodash/groupBy';
import { SkeletonList, Text } from '@hasura/shared/ui';
import { Em } from '@radix-ui/themes';
import { Source } from '@hasura/shared/types';
import useDatabaseTableParams from './useDatabaseTableParams';

type TreeViewProps = {
  sources: Source[];
  items: SourceItem[];
  databaseLoading: boolean;
  schemaLoading: boolean;
  preLoadState: boolean;
  gdcItemClick: (value: Key[]) => void;
};

const TreeView: React.FC<TreeViewProps> = ({
  sources,
  items,
  databaseLoading,
  schemaLoading,
  preLoadState,
}) => {
  const params = useDatabaseTableParams();

  const allDatabases = {
    ...sources.reduce(
      (acc, source) => {
        acc[source.name] = [];
        return acc;
      },
      {} as Record<string, SourceItem[]>,
    ),
    ...groupBy(items, 'source'),
  };

  if (!items.length) {
    return preLoadState ? (
      <SkeletonList count={5} />
    ) : (
      <Text as="div" data-test="sidebar-no-services">
        <Em>No data available</Em>
      </Text>
    );
  }

  return (
    <div>
      {Object.entries(allDatabases).map(([key, items]) => (
        <DatabaseItemsView
          items={items}
          key={key}
          sourceName={key}
          params={params}
          databaseLoading={databaseLoading}
          schemaLoading={schemaLoading}
        />
      ))}
    </div>
  );
};

export default TreeView;
