import React from 'react';
import { AiOutlineDown, AiOutlineRight } from 'react-icons/ai';
import { FaFolder, FaFolderOpen, FaTable } from 'react-icons/fa';
import { SourceItem } from '../types';
import LeafItemsView from './LeafItemsView';
import { Em, Flex, Skeleton } from '@radix-ui/themes';
import { Text } from '@hasura/shared/ui';

type SchemaItemsViewProps = {
  schemaName: string;
  items: SourceItem[];
  currentTable: string | null | undefined;
  isActive: boolean;
  setActiveSchema: (value: string) => void;
  databaseLoading: boolean;
  schemaLoading: boolean;
};

const SchemaItemsView: React.FC<SchemaItemsViewProps> = ({
  schemaName,
  items,
  currentTable,
  isActive,
  setActiveSchema,
  databaseLoading,
  schemaLoading,
}) => {
  return (
    <div className="pl-3 pb-2">
      <div
        onClick={() => {
          setActiveSchema(schemaName);
        }}
        onKeyDown={() => {
          setActiveSchema(schemaName);
        }}
        role="button"
        className="cursor-pointer my-2"
      >
        <Text
          color={isActive ? 'indigo' : 'gray'}
          weight={isActive ? 'medium' : 'regular'}
        >
          <Flex align="center" gap="2">
            {isActive ? <AiOutlineDown /> : <AiOutlineRight />}
            {isActive ? <FaFolderOpen /> : <FaFolder />} {schemaName}
          </Flex>
        </Text>
      </div>
      {isActive && items.length ? (
        !(databaseLoading || schemaLoading) ? (
          items.length ? (
            <Flex direction="column" gap="1">
              {items.map((child, key) => (
                <LeafItemsView
                  item={child}
                  isActive={isActive && currentTable === child.tableName}
                  key={key}
                />
              ))}
            </Flex>
          ) : (
            <Text data-test="table-sidebar-no-tables">
              <Em>No tables or views in this schema</Em>
            </Text>
          )
        ) : (
          <Flex align="center" gap="2">
            <FaTable />
            <Skeleton height="1rem" />
          </Flex>
        )
      ) : null}
    </div>
  );
};

export default SchemaItemsView;
