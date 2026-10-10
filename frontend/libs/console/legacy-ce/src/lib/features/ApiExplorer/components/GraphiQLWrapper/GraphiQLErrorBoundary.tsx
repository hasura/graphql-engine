import { clearGraphiqlLS } from '@hasura/shared/utils';
import React from 'react';

type State = {
  hasError: boolean;
  info: any;
};

type Props = {
  children?: React.ReactNode;
};

class GraphiQLErrorBoundary extends React.Component<Props, State> {
  constructor(props) {
    super(props);

    this.state = { hasError: false, info: null };
  }

  override componentDidCatch(_error, info) {
    this.setState({ hasError: true, info: info });
    // most likely a localstorage issue
    clearGraphiqlLS();
  }

  override render() {
    if (this.state.hasError) {
      // You can render any custom fallback UI
      return <div>{this.props.children}</div>;
    }
    return this.props.children;
  }
}

export default GraphiQLErrorBoundary;
