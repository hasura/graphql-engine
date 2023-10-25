import { SkeletonProps } from 'react-loading-skeleton';
import { APIError } from '../../../../hooks/error';
import { PartialBy } from '../../../../components/Common/utils/tsUtils';

// make the name property optional for easier use
export type ErrorType = PartialBy<APIError, 'name'> | null;

export type CommonProps<TData = unknown> = {
  renderError?: (params: { error: ErrorType }) => JSX.Element;
  renderIdle?: () => JSX.Element;
  fallbackData?: TData;
} & LoadingProps<TData> &
  ErrorProps<TData>;

type LoadingProps<TData = unknown> =
  | {
      // allow for no loading props at all
      loadingStyle?: never;
      loader?: never;
      renderLoading?: never;
    }
  | {
      //loadingStyle is optional for 'instead-of-content' as this is the default loadingStyle
      loadingStyle?: 'instead-of-content';
      loader: 'skeleton';
      skeletonProps: SkeletonProps;
    }
  | {
      loadingStyle?: 'instead-of-content';
      loader: 'spinner';
      miniSpinnerBackdrop?: boolean;
    }
  | {
      loadingStyle?: 'instead-of-content';
      loader?: never;
      renderLoading: () => JSX.Element;
    }
  | {
      loadingStyle: 'overlay';
      fallbackData: TData;
      loader: 'spinner';
      miniSpinnerBackdrop?: boolean;
    }
  | {
      loadingStyle: 'overlay';
      fallbackData: TData;
      loader: 'skeleton';
      skeletonCount: number;
    }
  | {
      loadingStyle: 'overlay';
      loader?: never;
      renderLoading: () => JSX.Element;
      fallbackData: TData;
    };

type ErrorProps<TData = unknown> =
  | { errorStyle?: 'instead-of-content' }
  | { errorStyle?: 'above-content'; fallbackData: TData }
  | { errorStyle?: 'below-content'; fallbackData: TData };
