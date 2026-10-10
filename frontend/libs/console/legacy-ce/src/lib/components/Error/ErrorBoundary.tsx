import React from 'react';
import PageNotFound, { NotFoundError } from './PageNotFound';
import RuntimeError from './RuntimeError';
import { trackRuntimeError } from '../../telemetry';
import type { Location, NavigateFunction } from 'react-router';
import { METADATA_STATUS_PATH } from '@hasura/shared/types';
import type { UseReloadMetadata } from '@hasura/metadata/api';

export interface ErrorBoundaryProps {
  location: Location;
  navigate: NavigateFunction;
  reloadMetadata: UseReloadMetadata['reloadMetadata'];
  children?: React.ReactNode;
}

interface ErrorBoundaryState {
  hasError: boolean;
  error: Error | null;
  type: string;
}

const initialState: ErrorBoundaryState = {
  hasError: false,
  error: null,
  type: '500',
};

class ErrorBoundary extends React.Component<
  ErrorBoundaryProps,
  ErrorBoundaryState
> {
  constructor(props: ErrorBoundaryProps) {
    super(props);

    this.state = initialState;
  }

  override componentDidCatch(error: Error) {
    const { reloadMetadata, navigate } = this.props;

    // ATTENTION: No need to setup anything for Sentry here, Sentry automatically tracks the error
    // caught from the error boundaries!

    // for invalid path segment errors
    if (error instanceof NotFoundError) {
      this.setState({
        type: '404',
      });
    }

    this.setState({ hasError: true, error });

    // trigger telemetry
    trackRuntimeError(error);
    console.error(error);

    reloadMetadata({})
      .then((isConsistent) => {
        if (!isConsistent) {
          if (!location.pathname.includes('/settings/metadata-status')) {
            this.resetState();
            navigate(METADATA_STATUS_PATH);
          }
        }
      })
      .catch((err) => {
        console.error('failed to reload metadata', err);
      });
  }

  resetState = () => {
    this.setState(initialState);
  };

  override render() {
    const { hasError, type, error } = this.state;

    if (hasError) {
      return type === '404' ? (
        <PageNotFound resetCallback={this.resetState} />
      ) : (
        <RuntimeError resetCallback={this.resetState} error={error} />
      );
    }

    return this.props.children;
  }
}

export default ErrorBoundary;
