import Permissions from './Main';
import { connect } from 'react-redux';

const mapStateToProps = state => {
  return {
    ...state.actions.permissions,
    allRoles: state.tables.allRoles,
    allActions: state.actions.common.actions,
  };
};

const ConnectedActionPermissions = connect(mapStateToProps)(Permissions);
export default ConnectedActionPermissions;
