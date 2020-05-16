import Manage from './Main';
import { connect } from 'react-redux';

const mapStateToProps = state => {
  return {
    allTypes: state.types.types,
    ...state.actions.types,
  };
};

const ConnectedManage = connect(mapStateToProps)(Manage);
export default ConnectedManage;
