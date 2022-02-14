import React from 'react';
import Spinner from '../Common/Spinner/Spinner';

import PageNotFound, { NotFoundError } from './PageNotFound';
import RuntimeError from './RuntimeError';
import { registerRunTimeError } from '../Main/Actions';
import { redirectToMetadataStatus } from '../Common/utils/routesUtils';
import { loadInconsistentObjects } from '../../metadata/actions';
import { Dispatch, FixMe } from '../../types';

export const isMetadataStatusPage = () => {
  return window.location.pathname.includes('/settings/metadata-status');
};

export interface Metadata {
  inconsistentObjects: Record<string, unknown>[];
  ongoingRequest: boolean;
  allowedQueries: Record<string, unknown>[];
}

export interface ErrorBoundaryProps {
  metadata: Metadata;
  dispatch: Dispatch;
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

  componentDidCatch(error: Error) {
    const { dispatch } = this.props;

    // for invalid path segment errors
    if (error instanceof NotFoundError) {
      this.setState({
        type: '404',
      });
    }

    this.setState({ hasError: true, error });

    // trigger telemetry
    dispatch(
      registerRunTimeError({ message: error.message, stack: error.stack })
    );

    dispatch(
      loadInconsistentObjects({ shouldReloadMetadata: true }) as FixMe
    ).then(() => {
      if (this.props.metadata.inconsistentObjects.length > 0) {
        if (!isMetadataStatusPage()) {
          this.resetState();
          this.props.dispatch(redirectToMetadataStatus());
        }
      } else {
        console.error(error);
      }
    });
  }

  resetState = () => {
    this.setState(initialState);
  };

  render() {
    const { metadata } = this.props;
    const { hasError, type, error } = this.state;

    if (hasError && metadata.ongoingRequest) {
      return <Spinner />;
    }

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
