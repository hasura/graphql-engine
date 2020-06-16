import AddAction from './Add';

const mapStateToProps = state => {
  return {
    ...state.actions.add,
    readOnlyMode: state.main.readOnlyMode,
  };
};

const connector = connect => connect(mapStateToProps)(AddAction);
export default connector;
