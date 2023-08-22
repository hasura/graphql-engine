import * as Dialog from '@radix-ui/react-dialog';
import { ReactNode } from 'react';
import { FiAlertTriangle } from 'react-icons/fi';
import Skeleton from 'react-loading-skeleton';
import { UseQueryResult } from 'react-query';
import { APIError } from '../../../../hooks/error';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { CenteredSpinner } from '../../../components';
import { CommonProps } from './types';

// Only for rendering loading / error UI given react query status/error object:
const ID_PREFIX = 'react-query-status-ui';

export const TestIds = {
  error: `${ID_PREFIX}-error-indicator`,
  loadingOverlay: `${ID_PREFIX}-loading-overlay`,
  spinner: `${ID_PREFIX}-loading-spinner`,
  skeleton: `${ID_PREFIX}-loading-skeleton`,
  childContent: `${ID_PREFIX}-child-content`,
  customLoading: `${ID_PREFIX}-custom-loading`,
  customError: `${ID_PREFIX}-custom-error`,
  customIdle: `${ID_PREFIX}-custom-idle`,
  errorAbove: `${ID_PREFIX}-error-above`,
  errorBelow: `${ID_PREFIX}-error-below`,
};

const twSkeletonOverley = `w-full h-full absolute inset-0 grid grid-flow-row dynamic-skeleton-grid gap-[2px] max-h-full overflow-hidden`;

export type ReactQueryStatusUIProps<
  TData = unknown,
  TError = unknown
> = CommonProps<TData> & {
  status: UseQueryResult['status'];
  error: TError;
  children?: ReactNode;
};
export function ReactQueryStatusUI<TData = unknown>({
  status,
  error: rawError,
  renderError,
  renderIdle,
  children,
  ...props
}: ReactQueryStatusUIProps<TData>) {
  const childContent = () => (
    <span data-testid={TestIds.childContent}>{children}</span>
  );

  const loadingUI = () => {
    if (props.loadingStyle === 'overlay') {
      if (!props.loader && props.renderLoading) {
        // custom loading rendered as overlay (below content)
        return (
          <span data-testid={TestIds.loadingOverlay} className="relative">
            <span data-testid={TestIds.customLoading}>
              {props.renderLoading()}
            </span>
            {childContent()}
          </span>
        );
      } else if (props.loader === 'skeleton') {
        // skeleton rendered with content. this option will overlay content
        // skeleton will be the exact height of childContent
        return (
          <div data-testid={TestIds.loadingOverlay} className={'relative'}>
            <div data-testid={TestIds.skeleton}>
              <style
                dangerouslySetInnerHTML={{
                  // prettier-ignore
                  __html: `.dynamic-skeleton-grid > br { display: none }`,
                }}
              />
              <Skeleton
                containerClassName={twSkeletonOverley}
                count={props.skeletonCount}
              />
            </div>
            <div className={'transition-all opacity-0 pointer-events-none'}>
              {childContent()}
            </div>
          </div>
        );
      } else {
        //render a spinner "with content" (over content like a modal)
        return (
          <span data-testid={TestIds.loadingOverlay}>
            <span data-testid={TestIds.spinner}>
              <Dialog.Root open>
                <Dialog.Content>
                  <CenteredSpinner
                    withMiniBackdrop={
                      props.loader === 'spinner' && props.miniSpinnerBackdrop
                    }
                  />
                </Dialog.Content>
              </Dialog.Root>
            </span>
            {childContent()}
          </span>
        );
      }
    }
    // default to loadingStyle 'instead-of-content'
    else {
      if (!props.loader && props.renderLoading) {
        //customer render with no child content
        return (
          <span data-testid={TestIds.customLoading}>
            {props.renderLoading()}
          </span>
        );
      } else if (props.loader === 'skeleton') {
        const skeletonProps = props.skeletonProps ?? {};
        // arbtrary skeleton instead of content
        return (
          <span data-testid={TestIds.skeleton}>
            <Skeleton {...skeletonProps} />
          </span>
        );
      } else {
        // centered spinner instead of content
        return (
          <span data-testid={TestIds.spinner}>
            <CenteredSpinner
              withMiniBackdrop={
                props.loader === 'spinner' && props.miniSpinnerBackdrop
              }
            />
          </span>
        );
      }
    }
  };

  const error = APIError.fromUnknown(rawError);

  // error rendered using an indicator card. is the default error display if nor renderError prop is given:
  const errorIndicator = () => (
    <span data-testid={TestIds.error}>
      <IndicatorCard
        // disabling margin to make this more agnostic
        className="mb-0"
        status="negative"
        showIcon
        customIcon={() => <FiAlertTriangle />}
      >
        {error.message}
      </IndicatorCard>
    </span>
  );

  const errorUI = () => {
    // 1st, check if the caller wants us to render the error alongside the children
    if (
      props.errorStyle === 'above-content' ||
      props.errorStyle === 'below-content'
    ) {
      const above = props.errorStyle === 'above-content';

      // render custom error above / below children
      if (renderError) {
        return (
          <span data-testid={above ? TestIds.errorAbove : TestIds.errorBelow}>
            {above && (
              <span data-testid={TestIds.customError}>
                {renderError({ error })}
              </span>
            )}
            {childContent()}
            {!above && (
              <span data-testid={TestIds.customError}>
                {renderError({ error })}
              </span>
            )}
          </span>
        );
      }

      // render default error display above / below children
      return (
        <span data-testid={above ? TestIds.errorAbove : TestIds.errorBelow}>
          {above && errorIndicator()}
          {childContent()}
          {!above && errorIndicator()}
        </span>
      );
    }

    // render custom error instead of children
    if (renderError) {
      return (
        <span data-testid={TestIds.customError}>{renderError({ error })}</span>
      );
    }

    // default behavior: render error on indicator card instead of children
    return errorIndicator();
  };

  const idleUI = () => {
    if (renderIdle) {
      return <span data-testid={TestIds.customIdle}>{renderIdle()}</span>;
    } else {
      return childContent();
    }
  };

  if (status === 'success') {
    return childContent();
  } else if (status === 'idle') {
    return idleUI();
  } else if (status === 'loading') {
    return loadingUI();
  } else {
    return errorUI();
  }
}
