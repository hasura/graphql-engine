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
  inconsistentObjects: object[];
  ongoingRequest: boolean;
  allowedQueries: object[];
}

export interface ErrorBoundaryProps {
  metadata: Metadata;
  dispatch: (arg: unknown) => Promise<unknown>; // TODO update when Redux is migrated to TS;
}

type ErrorBoundaryState = {
  hasError: boolean;
  error: Error | null;
  type: string;
};

class ErrorBoundary extends React.Component<
  ErrorBoundaryProps,
  ErrorBoundaryState
> {
  constructor(props: ErrorBoundaryProps) {
    super(props);

    this.state = this.initialState;
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

  initialState = {
    hasError: false,
    error: null,
    type: '500',
  };

  resetState = () => {
    this.setState({ ...this.initialState });
  };

  render() {
    const { metadata } = this.props;
    const { hasError, type, error } = this.state;

    if (hasError && metadata.ongoingRequest) {
      return (
        <div>
          <Spinner />
        </div>
      );
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
