import React from 'react';
import {
  loadInconsistentObjects,
  redirectToMetadataStatus,
  isMetadataStatusPage,
} from '../Services/Settings/Actions';
import Spinner from '../Common/Spinner/Spinner';

import PageNotFound, { NotFoundError } from './PageNotFound';
import RuntimeError from './RuntimeError';
import { registerRunTimeError } from '../Main/Actions';

export interface Metadata {
  inconsistentObjects: Record<string, unknown>[];
  ongoingRequest: boolean;
  allowedQueries: Record<string, unknown>[];
}

export interface ErrorBoundaryProps {
  metadata: Metadata;
  dispatch: (arg: unknown) => Promise<unknown>; // TODO update when Redux is migrated to TS;
  errorValue: Error;
  requestError: Error;
  requestURL: string;
}

interface ErrorBoundaryState {
  hasReactError: boolean;
  error: Error | null;
  type: string;
}

const initialState: ErrorBoundaryState = {
  hasReactError: false,
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

    this.setState({ hasReactError: true, error });

    // trigger telemetry
    dispatch(
      registerRunTimeError({ message: error.message, stack: error.stack })
    );

    dispatch(loadInconsistentObjects({ shouldReloadMetadata: true })).then(
      () => {
        if (this.props.metadata.inconsistentObjects.length > 0) {
          if (!isMetadataStatusPage()) {
            this.resetState();
            this.props.dispatch(redirectToMetadataStatus());
          }
        } else {
          console.error(error);
        }
      }
    );
  }

  resetState = () => {
    this.setState(initialState);
  };

  render() {
    const { metadata } = this.props;
    const { hasReactError, type, error } = this.state;

    if (hasReactError && metadata.ongoingRequest) {
      return <Spinner />;
    }

    if (hasReactError) {
      return type === '404' ? (
        <PageNotFound resetCallback={this.resetState} />
      ) : (
        <RuntimeError resetCallback={this.resetState} error={error} />
      );
    }

    // Catch Api errors

    return this.props.children;
  }
}

export default ErrorBoundary;
