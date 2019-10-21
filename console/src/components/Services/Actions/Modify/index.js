import Modify from './Main';

const mapStateToProps = state => {
  return {
    ...state.actions.permissions,
  };
};

const connector = connect => connect(mapStateToProps)(Modify);
export default connector;
