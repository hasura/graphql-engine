import Landing from './Main';

const mapStateToProps = state => {
  return {
    ...state.actions.list,
  };
};

const connector = connect => connect(mapStateToProps)(Landing);
export default connector;
