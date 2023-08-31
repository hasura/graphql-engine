// a helper fn to cover the cases when the ui should render in non-success statuses

import { UseQueryResult } from 'react-query';
import { ReactQueryStatusUIProps } from './ReactQueryStatusUI';

// written in a slightly verbose manner for clarity
export const nonSuccessRenderChildren = ({
  status,
  loadingStyle,
  errorStyle,
}: Omit<ReactQueryStatusUIProps<unknown>, 'error'>) => {
  if (status === 'idle') return true;
  if (status === 'loading' && loadingStyle === 'overlay') {
    return true;
  }
  if (
    status === 'error' &&
    (errorStyle === 'above-content' || errorStyle === 'below-content')
  ) {
    return true;
  }
  return false;
};

type MultipleQueryResults = UseQueryResult<unknown, unknown>[];

export const multipleQueryUtils = {
  status: (results: MultipleQueryResults): UseQueryResult['status'] => {
    const statuses = results.map(r => r.status);
    // prioritize error. if any statuses are error, then status is error
    if (statuses.some(s => s === 'error')) return 'error';

    // next, prioritize loading. if any are loading, then status is loading
    if (statuses.some(s => s === 'loading')) return 'loading';

    // next, idle
    if (statuses.some(s => s === 'idle')) return 'idle';

    // if we got here, then all statuses are success!
    return 'success';
  },
  firstError: (results: MultipleQueryResults): unknown => {
    return results.find(r => r.isError)?.error ?? null;
  },
  allErrors: (results: MultipleQueryResults): unknown[] => {
    return results.filter(r => r.isError).map(r => r.error);
  },
};
