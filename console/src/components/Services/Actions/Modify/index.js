import Modify from './Main';
import { connect } from 'react-redux';

const mapStateToProps = state => {
  return {
    ...state.actions.modify,
    allActions: state.actions.common.actions,
    allTypes: state.types.types,
  };
};

const ConnectedModify = connect(mapStateToProps)(Modify);
export default ConnectedModify;
