import Permissions from './Main';

const mapStateToProps = state => {
  return {
    ...state.actions.permissions,
    allActions: state.actions.common.actions,
  };
};

const connector = connect => connect(mapStateToProps)(Permissions);
export default connector;
