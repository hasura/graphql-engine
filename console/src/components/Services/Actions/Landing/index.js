import Landing from './Main';

const mapStateToProps = state => {
  return {
    ...state.actions.common,
  };
};

const connector = connect => connect(mapStateToProps)(Landing);
export default connector;
