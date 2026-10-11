import React from 'react';
import { useNavigate } from 'react-router';
import { FaArrowRight, FaFlask } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import {
  Badge,
  DropdownButton,
  DropdownMenu,
  Tooltip,
} from '@hasura/shared/ui';
import { Analytics } from '@hasura/shared/analytics';
import { LS_KEYS, MetadataTable, Source } from '@hasura/shared/types';
import { getTableDisplayName, setLSItem } from '@hasura/shared/utils';
import { useIntrospectSchema } from '@hasura/metadata/api';
import {
  getDatabaseMethods,
  useTableColumns,
} from '@hasura/metadata/data-source';
import {
  generateGqlQueryFromTable,
  getTableRootFields,
  OperationType,
} from './utils';

type Props = {
  source: Source;
  metadataTable: MetadataTable;
};

const operations: {
  type: OperationType;
  label: string;
  analyticsName: string;
  isNew?: boolean;
  unavailableReason: string;
}[] = [
  {
    type: 'query',
    label: 'Query',
    analyticsName: 'data-tab-btn-try-query',
    unavailableReason: 'Queries are not exposed for this table.',
  },
  {
    type: 'mutation',
    label: 'Mutation',
    analyticsName: 'data-tab-btn-try-streaming-mutation',
    unavailableReason:
      'Insert mutations are not available for this table or data source.',
  },
  {
    type: 'subscription',
    label: 'Subscription - Live query',
    analyticsName: 'data-tab-btn-try-subscriptions',
    unavailableReason:
      'Subscriptions are not available for this table or data source.',
  },
  {
    type: 'streaming_subscription',
    label: 'Subscription - Streaming',
    analyticsName: 'data-tab-btn-try-streaming-subscriptions',
    isNew: true,
    unavailableReason:
      'Streaming subscriptions are not available for this table because the data source does not support them or there is no valid field to use as the initial value.',
  },
];

export const TryOperation = ({ source, metadataTable }: Props) => {
  const navigate = useNavigate();
  const { data: schema, isLoading: isSchemaLoading } = useIntrospectSchema();
  const { data: tableColumns, isLoading: isColumnsLoading } = useTableColumns({
    source,
    table: metadataTable.table,
  });

  const queries = React.useMemo(() => {
    if (!schema) return undefined;

    const rootFields = getTableRootFields({
      schema,
      defaultQueryRoot: getDatabaseMethods(
        source.kind,
      ).config.getDefaultQueryRoot({ table: metadataTable.table }),
      configuration: metadataTable.configuration,
      sourceCustomization: source.customization,
    });

    return Object.fromEntries(
      operations.map(({ type }) => [
        type,
        generateGqlQueryFromTable({
          operationType: type,
          tableName:
            metadataTable.configuration?.custom_name ??
            getTableDisplayName(metadataTable.table),
          rootFields,
          columns: tableColumns?.columns ?? [],
          configuration: metadataTable.configuration,
        }),
      ]),
    ) as Partial<Record<OperationType, string>>;
  }, [schema, source, metadataTable, tableColumns]);

  const tryOnGQL = (query: string) => () => {
    setLSItem(LS_KEYS.graphiqlQuery, query);
    navigate('/api/api-explorer');
  };

  return (
    <DropdownButton
      mode="default"
      size="sm"
      leftIcon={FaFlask}
      disabled={isSchemaLoading || isColumnsLoading || !queries}
      items={[
        <DropdownMenu.Label key="label">
          <span className="text-xs font-semibold uppercase tracking-wider">
            Try it in GraphiQL
          </span>
        </DropdownMenu.Label>,
        ...operations.map((operation) => {
          const query = queries?.[operation.type];
          const item = (
            <Analytics
              name={operation.analyticsName}
              passHtmlAttributesToChildren
            >
              <DropdownMenu.Item
                disabled={!query}
                onSelect={query ? tryOnGQL(query) : undefined}
              >
                <Flex
                  align="center"
                  justify="between"
                  gap="4"
                  className="w-full"
                >
                  <Flex align="center" gap="2">
                    {operation.label}
                    {operation.isNew && <Badge color="yellow">New</Badge>}
                  </Flex>
                  <FaArrowRight className="w-3 h-3 opacity-60" />
                </Flex>
              </DropdownMenu.Item>
            </Analytics>
          );

          return query ? (
            <React.Fragment key={operation.type}>{item}</React.Fragment>
          ) : (
            <Tooltip
              key={operation.type}
              side="right"
              content={operation.unavailableReason}
            >
              {item}
            </Tooltip>
          );
        }),
      ]}
    >
      Try GraphQL
    </DropdownButton>
  );
};
