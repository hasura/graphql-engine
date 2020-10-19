import Permissions from './Main';

const mapStateToProps = state => {
  return {
    ...state.remoteSchemas.permissions,
    allRoles: state.tables.allRoles,
    allRemoteSchemas: state.remoteSchemas.listData.remoteSchemas,
    readOnlyMode: state.main.readOnlyMode,
  };
};

// TODO
export default connect => connect(mapStateToProps)(Permissions);
