import Manage from './Main';

const mapStateToProps = state => {
  return {
    allTypes: state.types.types,
    readOnlyMode: state.main.readOnlyMode,
    ...state.actions.types,
  };
};

const connector = connect => connect(mapStateToProps)(Manage);
export default connector;
