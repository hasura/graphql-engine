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

export type DefaultState = {
  inconsistentObjects: Array<object>;
  ongoingRequest: boolean;
  allowedQueries: Array<object>;
}

export interface ErrorBoundaryProps {
  metadata: DefaultState;
  dispatch: (arg: object) => Promise<object>;
}

type ErrorBoundaryState = {
  hasError: boolean;
  info: object | null;
  error: Error | null;
  type: string;
} 

class ErrorBoundary extends React.Component<ErrorBoundaryProps> {
  initialState: ErrorBoundaryState = {
    hasError: false,
    info: null,
    error: null,
    type: '500',
  };

  constructor(props: ErrorBoundaryProps) {
    super(props);

    this.state = this.initialState;
  }

  resetState = () => {
    this.setState({ ...this.initialState });
  };

  componentDidCatch(error: Error, info: object) {
    const { dispatch } = this.props;

    // for invalid path segment errors
    if (error instanceof NotFoundError) {
      this.setState({
        type: '404',
      });
    }

    this.setState({ hasError: true, info: info, error: error });

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

  render() {
    const { metadata } = this.props;
    const { hasError, type, error } = this.state as ErrorBoundaryState;

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
