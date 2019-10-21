import AddAction from './Main';

const mapStateToProps = state => {
  return {
    ...state.actions.add,
  };
};

const connector = connect => connect(mapStateToProps)(AddAction);
export default connector;
