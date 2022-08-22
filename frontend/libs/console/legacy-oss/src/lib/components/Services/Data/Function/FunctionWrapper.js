import React from 'react';
import PropTypes from 'prop-types';

import { RESET } from './customFunctionReducer';

import { setTable } from '../DataActions';
import { getFunctionConfiguration } from '../../../../metadata/selector';
import { RightContainer } from '../../../Common/Layout/RightContainer';

// TODO: GET RID OF THIS ASAP
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
    return (
      <RightContainer>
        {children && React.cloneElement(children, this.props)}
      </RightContainer>
    );
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
      configuration: getFunctionConfiguration(state),
    },
    currentSource: state.tables.currentDataSource,
    migrationMode: state.main.migrationMode,
  };
};

const functionWrapperConnector = connect =>
  connect(mapStateToProps)(FunctionWrapper);

export default functionWrapperConnector;
