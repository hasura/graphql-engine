import Details from './Main';

const mapStateToProps = state => {
  return {
    ...state.actions.details,
  };
};

const connector = connect => connect(mapStateToProps)(Details);
export default connector;
