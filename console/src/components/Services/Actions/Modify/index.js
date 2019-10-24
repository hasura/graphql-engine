import Modify from './Main';

const mapStateToProps = state => {
  return {
    ...state.actions.modify,
    allActions: state.actions.common.actions,
  };
};

const connector = connect => connect(mapStateToProps)(Modify);
export default connector;
