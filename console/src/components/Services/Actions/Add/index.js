import AddAction from './Add';

const mapStateToProps = state => {
  return {
    ...state.actions.add,
  };
};

const connector = connect => connect(mapStateToProps)(AddAction);
export default connector;
