import { Options, useMetadata } from '@hasura/metadata/api';
import { Metadata } from '@hasura/shared/types';
import { ReactQueryUIWrapper } from './ReactQueryUIWrapper';
import { CommonProps } from './types';
import type { JSX } from 'react';
const ID_PREFIX = 'metadata-provider';

export const TestIds = {
  renderContent: ID_PREFIX + '-render-content',
};

const DEFAULT_STALE_TIME = 5 * 60000; // 5 minutes as default stale time

/**
 *
 * A component that executes the useMetadata hook with built in loading/error handling for the UI. You can pass in a custom selector function or have just return all of the metadata.
 * You may optionally pass in custom render props for renderError or renderLoading
 *
 */
export function MetadataWrapper<FinalResult = Metadata>({
  render,
  selector,
  options = {
    staleTime: DEFAULT_STALE_TIME,
    enabled: true,
  },
  ...props
}: {
  render: (params: { data: FinalResult }) => JSX.Element;
  selector?: (m: Metadata) => FinalResult;
  options?: Options;
} & CommonProps<FinalResult>) {
  const result = useMetadata(selector, options);

  return (
    <ReactQueryUIWrapper useQueryResult={result} render={render} {...props} />
  );
}
