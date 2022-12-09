import Landing from './Main';

const mapStateToProps = state => {
  return {
    ...state.actions.common,
    readOnlyMode: state.main.readOnlyMode,
  };
};

const connector = connect => connect(mapStateToProps)(Landing);
export default connector;
