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
            <Flex gap="2" align="center" className="min-w-0">
              <TbMathFunction className="shrink-0" />
              <span className="truncate" title={item.tableName}>
                {item.tableName}
              </span>
            </Flex>
          </LeftSidebarLeafNavItem>
        ) : (
          <Flex align="center" gap="2">
            <div className="min-w-0 flex-1">
              <LeftSidebarLeafNavItem
                isActive={isActive}
                to={dataRoutes.manageTable(item.source, item.table)}
              >
                <Flex align="center" gap="2" className="min-w-0">
                  {item.type === 'enum' ? (
                    <FaListUl className="shrink-0" />
                  ) : (
                    <FaTable className="shrink-0" />
                  )}
                  {item.type === 'view' ? (
                    <Em className="truncate" title={item.tableName}>
                      {item.tableName}
                    </Em>
                  ) : (
                    <span className="truncate" title={item.tableName}>
                      {item.tableName}
                    </span>
                  )}
                </Flex>
              </LeftSidebarLeafNavItem>
            </div>
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
