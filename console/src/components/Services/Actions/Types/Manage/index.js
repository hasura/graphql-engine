import Manage from './Main';

const mapStateToProps = state => {
  return {
    allTypes: state.types.types,
    ...state.actions.types,
  };
};

const connector = connect => connect(mapStateToProps)(Manage);
export default connector;
