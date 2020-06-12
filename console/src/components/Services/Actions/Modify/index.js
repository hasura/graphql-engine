import Modify from './Main';

const mapStateToProps = state => {
  return {
    ...state.actions.modify,
    allActions: state.actions.common.actions,
    allTypes: state.types.types,
    readOnlyMode: state.main.readOnlyMode,
  };
};

const connector = connect => connect(mapStateToProps)(Modify);
export default connector;
