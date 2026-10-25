import { AiOutlineDown, AiOutlineRight } from 'react-icons/ai';
import { ReactNode } from 'react';
import { SourceItem, SourceItemsTypes } from '../types';
import { FaDatabase, FaFolder, FaListUl, FaTable } from 'react-icons/fa';
import SchemaItemsView from './SchemaItemsView';
import LeafItemsView from './LeafItemsView';
import groupBy from 'lodash/groupBy';
import partition from 'lodash/partition';
import { Flex, Skeleton, Strong } from '@radix-ui/themes';
import { Button } from '@hasura/shared/ui';
import { useNavigate } from 'react-router';
import { dataRoutes } from '@hasura/shared/utils';
import { DatabaseTableParamsReturn } from './useDatabaseTableParams';

type DatabaseItemsViewProps = {
  sourceName: string;
  params: DatabaseTableParamsReturn;
  items: SourceItem[];
  databaseLoading: boolean;
  schemaLoading: boolean;
};

const sourceItemsTypesToIcons: Record<SourceItemsTypes, ReactNode> = {
  enum: <FaListUl className="shrink-0" />,
  database: <FaDatabase className="shrink-0" />,
  function: <FaDatabase className="shrink-0" />,
  schema: null,
  table: <FaTable className="shrink-0" />,
  view: null,
};

const DatabaseItemsView: React.FC<DatabaseItemsViewProps> = ({
  params,
  sourceName,
  items,
  databaseLoading,
  schemaLoading,
}) => {
  const navigate = useNavigate();
  const isActive = params.source === sourceName;

  const handleSelectSchema = (value: string) => {
    navigate({
      pathname: dataRoutes.manageDatabaseSource(sourceName),
      search: `?schema=${encodeURIComponent(value)}&tab=tables`,
    });
  };

  const onDatabaseChange = () => {
    navigate(dataRoutes.manageDatabaseSource(sourceName));
  };

  // objects of databases without schemas (e.g. MongoDB collections) are listed
  // under the database, not in an unnamed schema folder
  const [schemaItems, schemalessItems] = partition(
    items,
    (item) => !!item.schema,
  );
  const schemas = groupBy(schemaItems, 'schema');

  return (
    <div>
      <div
        role="button"
        className="cursor-pointer"
        onClick={onDatabaseChange}
        onKeyDown={onDatabaseChange}
      >
        <Button
          color={isActive ? 'indigo' : 'gray'}
          variant="ghost"
          className="justify-start! text-left!"
          full
        >
          <Flex align="center" gap="2" className="min-w-0">
            {isActive ? (
              <AiOutlineDown className="shrink-0" />
            ) : (
              <AiOutlineRight className="shrink-0" />
            )}
            {sourceItemsTypesToIcons['database']}{' '}
            <Strong className="truncate">{sourceName}</Strong>
          </Flex>
        </Button>
      </div>
      {isActive && items.length
        ? Object.entries(schemas).map(([schemaName, schemaItems]) => (
            <SchemaItemsView
              key={schemaName}
              items={schemaItems}
              currentTable={params.tableName}
              isCurrentFunction={params.isFunction}
              isActive={schemaName === params.schema}
              setActiveSchema={handleSelectSchema}
              schemaName={schemaName}
              databaseLoading={databaseLoading}
              schemaLoading={schemaLoading}
            />
          ))
        : null}
      {isActive && schemalessItems.length ? (
        <Flex direction="column" gap="1" className="mb-2">
          {schemalessItems.map((item, key) => (
            <LeafItemsView
              item={item}
              isActive={
                !params.schema &&
                params.tableName === item.tableName &&
                params.isFunction === (item.type === 'function')
              }
              key={key}
            />
          ))}
        </Flex>
      ) : null}
      {databaseLoading && isActive ? (
        <Flex align="center" gap="2">
          <FaFolder />
          <Skeleton height="14px" />
        </Flex>
      ) : null}
    </div>
  );
};

export default DatabaseItemsView;
