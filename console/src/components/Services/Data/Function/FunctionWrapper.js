import React from 'react';
import PropTypes from 'prop-types';

class FunctionWrapper extends React.Component {
  render() {
    const { children } = this.props;
    return <div>{children && React.cloneElement(children, this.props)}</div>;
  }
}

FunctionWrapper.propTypes = {
  children: PropTypes.node,
};

const mapStateToProps = state => {
  return {
    functionList: state.tables.postgresFunctions,
    functions: {
      ...state.functions,
    },
  };
};

const functionWrapperConnector = connect =>
  connect(mapStateToProps)(FunctionWrapper);

export default functionWrapperConnector;
