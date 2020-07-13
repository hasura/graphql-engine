import Permissions from './Main';

const mapStateToProps = state => {
  return {
    ...state.actions.permissions,
    allRoles: state.tables.allRoles,
    allActions: state.actions.common.actions,
    readOnlyMode: state.main.readOnlyMode,
  };
};

const connector = connect => connect(mapStateToProps)(Permissions);
export default connector;
