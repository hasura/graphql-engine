import React from 'react';
import PropTypes from 'prop-types';

import { RESET } from './customFunctionReducer';

import { setTable } from '../DataActions';
import { connect } from 'react-redux';

class FunctionWrapper extends React.Component {
  componentDidMount() {
    this.props.dispatch(setTable(''));
  }
  componentWillUnmount() {
    this.props.dispatch({
      type: RESET,
    });
  }
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
    migrationMode: state.main.migrationMode,
  };
};

const ConnectedFunctionWrapper = connect(mapStateToProps)(FunctionWrapper);
export default ConnectedFunctionWrapper;
