import Details from './Main';

const mapStateToProps = state => {
  return {
    ...state.actions.details,
    allActions: state.actions.common.actions,
  };
};

const connector = connect => connect(mapStateToProps)(Details);
export default connector;
