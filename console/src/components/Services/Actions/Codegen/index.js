import Codegen from './Main';
import { connect } from 'react-redux';

const mapStateToProps = state => {
  return {
    ...state.actions.relationships,
    allActions: state.actions.common.actions,
    allTypes: state.types.types,
  };
};

const ConnectedCodegen = connect(mapStateToProps)(Codegen);
export default ConnectedCodegen;
