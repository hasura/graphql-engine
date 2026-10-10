import React from 'react';
import { FaListUl, FaTable } from 'react-icons/fa';
import { SourceItem } from '../types';
import { dataRoutes } from '@hasura/shared/utils';
import {
  GqlCompatibilityWarning,
  LeftSidebarLeafNavItem,
} from '@hasura/shared/ui';
import { Em, Flex } from '@radix-ui/themes';
import { TbMathFunction } from 'react-icons/tb';

type LeafItemsViewProps = {
  item: SourceItem;
  isActive: boolean;
};

const LeafItemsView: React.FC<LeafItemsViewProps> = ({ item, isActive }) => {
  return (
    <div className="cursor-pointer pl-6" role="button">
      <span>
        {item.type === 'function' ? (
          <LeftSidebarLeafNavItem
            isActive={isActive}
            to={dataRoutes.manageFunction(item.source, item.table)}
          >
            <Flex gap="2" align="center">
              <TbMathFunction />
              {item.tableName}
            </Flex>
          </LeftSidebarLeafNavItem>
        ) : (
          <Flex align="center" gap="2">
            <LeftSidebarLeafNavItem
              isActive={isActive}
              to={dataRoutes.manageTable(item.source, item.table)}
            >
              <Flex align="center" gap="2">
                {item.type === 'enum' ? <FaListUl /> : <FaTable />}
                {item.type === 'view' ? (
                  <Em>{item.tableName}</Em>
                ) : (
                  item.tableName
                )}
              </Flex>
            </LeftSidebarLeafNavItem>
            <GqlCompatibilityWarning
              identifier={item.tableName}
              ifWarningCanBeFixed
            />
          </Flex>
        )}
      </span>
    </div>
  );
};

export default LeafItemsView;
