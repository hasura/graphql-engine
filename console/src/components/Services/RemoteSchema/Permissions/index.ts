import Permissions from './Main';

const mapStateToProps = (state: any) => {
  return {
    ...state.remoteSchemas.permissions,
    allRoles: state.tables.allRoles,
    allRemoteSchemas: state.remoteSchemas.listData.remoteSchemas,
    readOnlyMode: state.main.readOnlyMode,
  };
};

export default (connect: any) => connect(mapStateToProps)(Permissions);
