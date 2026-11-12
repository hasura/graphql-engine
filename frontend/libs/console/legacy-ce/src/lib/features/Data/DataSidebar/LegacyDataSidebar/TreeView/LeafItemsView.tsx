import React from 'react';
import { FaListUl, FaTable } from 'react-icons/fa';
import { SourceItem } from '../types';
import { dataRoutes } from '@hasura/shared/utils';
import {
  GqlCompatibilityWarning,
  LeftSidebarLeafNavItem,
  WarningSymbol,
} from '@hasura/shared/ui';
import { Em, Flex } from '@radix-ui/themes';
import { TbMathFunction } from 'react-icons/tb';

type LeafItemsViewProps = {
  item: SourceItem;
  isActive: boolean;
};

/** The metadata of the item no longer matches the database. */
const InconsistencyWarning = ({ reason }: { reason?: string }) =>
  reason ? (
    <span data-testid="sidebar-inconsistency-warning">
      <WarningSymbol
        tooltipText={`${reason} (see Settings > Metadata Status)`}
      />
    </span>
  ) : null;

const METADATA_STATUS_ROUTE = '/settings/metadata-status';

const LeafItemsView: React.FC<LeafItemsViewProps> = ({ item, isActive }) => {
  // the pages of an inconsistent item can't load it, so open the list of
  // inconsistent objects to resolve it instead
  const to = item.inconsistency
    ? METADATA_STATUS_ROUTE
    : item.type === 'function'
      ? dataRoutes.manageFunction(item.source, item.table)
      : dataRoutes.manageTable(item.source, item.table, 'browse');

  return (
    <div className="cursor-pointer pl-6" role="button">
      <span>
        {item.type === 'function' ? (
          <Flex align="center" gap="2">
            <div className="min-w-0 flex-1">
              <LeftSidebarLeafNavItem isActive={isActive} to={to}>
                <Flex gap="2" align="center" className="min-w-0">
                  <TbMathFunction className="shrink-0" />
                  <span className="truncate" title={item.tableName}>
                    {item.tableName}
                  </span>
                </Flex>
              </LeftSidebarLeafNavItem>
            </div>
            <InconsistencyWarning reason={item.inconsistency} />
          </Flex>
        ) : (
          <Flex align="center" gap="2">
            <div className="min-w-0 flex-1">
              <LeftSidebarLeafNavItem isActive={isActive} to={to}>
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
            <InconsistencyWarning reason={item.inconsistency} />
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
