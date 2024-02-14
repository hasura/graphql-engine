import React from 'react';
import clsx from 'clsx';
import { Dispatch } from 'redux';
import isEmpty from 'lodash/isEmpty';
import { Badge } from '../../../new-components/Badge';
import { DropdownButton } from '../../../new-components/DropdownButton';
import { FaArrowRight, FaFlask } from 'react-icons/fa';
import { LS_KEYS, setLSItem } from '../../../utils/localStorage';
import { Tooltip } from '../../../new-components/Tooltip';
import { Analytics } from '../../../features/Analytics';
import { useMetadataSource } from '../../../features/MetadataAPI';
import {
  getInitialValueField,
  generateGqlQueryFromTable,
  OperationType,
} from './utils';
import { Table } from '../../../dataSources/types';
import _push from '../../Services/Data/push';

type Props = {
  table: Table;
  dispatch: Dispatch;
  source: string;
};

export const TryOperation = (props: Props) => {
  const { table, dispatch, source } = props;
  const { data: dbInfo } = useMetadataSource(source);
  const tryOnGQL = (queryType: OperationType) => () => {
    const { query, variables } = generateGqlQueryFromTable(queryType, table);
    setLSItem(LS_KEYS.graphiqlQuery, query);
    setLSItem(LS_KEYS.graphiqlVariables, JSON.stringify(variables));
    dispatch(_push('/api/api-explorer'));
  };

  const isStreamingSubscriptionAvailable = !!getInitialValueField(table);
  const isTryitButtonAvailable =
    isEmpty(table?.configuration) &&
    isEmpty(dbInfo?.customization?.root_fields) &&
    isEmpty(dbInfo?.customization?.type_names) &&
    dbInfo?.customization?.naming_convention !== 'graphql-default';

  const streamingSubscripton = (
    <Analytics name="data-tab-btn-try-streaming-subscriptions">
      <div
        onClick={
          isStreamingSubscriptionAvailable
            ? tryOnGQL('streaming_subscription')
            : undefined
        }
        className={clsx(
          'py-xs w-full flex justify-between align-center',
          isStreamingSubscriptionAvailable
            ? 'cursor-pointer'
            : 'cursor-not-allowed text-muted'
        )}
      >
        Subscription - Streaming
        <Badge className="mx-2" color="yellow">
          New
        </Badge>
        <div className="text-muted">
          <FaArrowRight className="w-3 h-3" />
        </div>
      </div>
    </Analytics>
  );
  if (isTryitButtonAvailable) {
    return (
      <DropdownButton
        className="ml-xs"
        items={[
          [
            <span className="py-xs text-xs font-semibold text-muted uppercase tracking-wider whitespace-nowrap">
              Try it in GraphiQL
            </span>,
            <Analytics name="data-tab-btn-try-query">
              <div
                onClick={tryOnGQL('query')}
                className="py-xs w-full flex justify-between align-center"
              >
                Query
                <div className="text-muted">
                  <FaArrowRight className="w-3 h-3" />
                </div>
              </div>
            </Analytics>,
            <Analytics name="data-tab-btn-try-streaming-mutation">
              <div
                onClick={tryOnGQL('mutation')}
                className="py-xs w-full flex justify-between align-center"
              >
                Mutation
                <div className="text-muted">
                  <FaArrowRight className="w-3 h-3" />
                </div>
              </div>
            </Analytics>,
            <Analytics name="data-tab-btn-try-subscriptions">
              <div
                onClick={tryOnGQL('subscription')}
                className="py-xs w-full flex justify-between align-center"
              >
                Subscription - Live query
                <div className="text-muted">
                  <FaArrowRight className="w-3 h-3" />
                </div>
              </div>
            </Analytics>,
            isStreamingSubscriptionAvailable ? (
              streamingSubscripton
            ) : (
              <Tooltip
                className="ml-0"
                tooltipContentChildren="Streaming subscriptions are not available for this table because there is no valid field to use as the initial value."
              >
                {streamingSubscripton}
              </Tooltip>
            ),
          ],
        ]}
      >
        <FaFlask className="mr-xs" />
        Try GraphQL
      </DropdownButton>
    );
  }
  return (
    <Tooltip
      className="ml-xs"
      tooltipContentChildren='"Try it" is disabled for this table because it has customizations. Head to API Explorer to try out a query.'
    >
      <DropdownButton
        items={[]}
        className={clsx(
          'py-xs w-full flex justify-between align-center cursor-not-allowed text-muted'
        )}
        onClick={undefined}
      >
        <FaFlask className="mr-xs" />
        Try GraphQL
      </DropdownButton>
    </Tooltip>
  );
};
