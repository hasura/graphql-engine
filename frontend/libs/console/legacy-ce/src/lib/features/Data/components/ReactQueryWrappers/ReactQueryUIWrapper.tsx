import { UseQueryResult } from '@tanstack/react-query';
import { ReactQueryStatusUI } from './ReactQueryStatusUI';
import { CommonProps } from './types';
import { nonSuccessRenderChildren } from './utils';

import type { JSX } from 'react';

// pass the useQueryReturn to this component and it will...
// 1. render error/loading UI based on props or defaults
// 2. pass a non-nullable non-undefined type of whatever is returned from your query to a render prop so you dont have to deal with handling if the data is undefined in your main component
const ID_PREFIX = 'react-query-ui-provider';

export const TestIds = {
  renderContent: ID_PREFIX + '-render-content',
};

export const IdleQueryErrorMessage = `Query is idle and with no fallbackData or renderIdle prop. Please provide the fallbackData or renderIdle prop to ReactQueryUIWrapper for a useQuery() implementation that starts out { enabled: false }.`;

export function ReactQueryUIWrapper<TData = unknown>({
  useQueryResult,
  render,
  ...props
}: CommonProps<TData> & {
  useQueryResult: UseQueryResult<TData, unknown>;
  render: (params: { data: TData }) => JSX.Element;
}) {
  const { status, fetchStatus, data, error } = useQueryResult;
  const isIdle = status === 'pending' && fetchStatus === 'idle';

  // idle will get rendred so we need fallback data to safely render the render() prop
  if (isIdle && !props.fallbackData && !props.renderIdle) {
    return (
      <ReactQueryStatusUI
        status={'error'}
        error={{
          message: IdleQueryErrorMessage,
        }}
      />
    );
  }

  const uiStatus = isIdle ? 'idle' : status;

  return (
    // ReactQueryStatusUI will attempt to render the children whenever the above const alsoRendersChildren === true
    <ReactQueryStatusUI status={uiStatus} error={error} {...props}>
      <span data-testid={TestIds.renderContent}>
        {status === 'success' && render({ data })}
        {nonSuccessRenderChildren({ ...props, status: uiStatus }) &&
          props.fallbackData &&
          render({ data: props.fallbackData })}
      </span>
    </ReactQueryStatusUI>
  );
}
