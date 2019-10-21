import Permissions from './Main';

const mapStateToProps = state => {
  return {
    ...state.actions.permissions,
  };
};

const connector = connect => connect(mapStateToProps)(Permissions);
export default connector;
