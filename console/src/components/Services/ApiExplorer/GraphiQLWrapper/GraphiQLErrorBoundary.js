import React from 'react';
import PropTypes from 'prop-types';
import { clearGraphiqlLS } from '../../../../utils/localStorage';

class GraphiQLErrorBoundary extends React.Component {
  constructor(props) {
    super(props);
    this.state = { hasError: false, info: null };
  }

  componentDidCatch(_error, info) {
    this.setState({ hasError: true, info: info });
    // most likely a localstorage issue
    clearGraphiqlLS();
  }

  render() {
    if (this.state.hasError) {
      // You can render any custom fallback UI
      return <div>{this.props.children}</div>;
    }
    return this.props.children;
  }
}

GraphiQLErrorBoundary.propTypes = {
  children: PropTypes.element,
};

export default GraphiQLErrorBoundary;
