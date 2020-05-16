import AddAction from './Add';
import { connect } from 'react-redux';

const mapStateToProps = state => {
  return {
    ...state.actions.add,
  };
};

const ConnectedAddAction = connect(mapStateToProps)(AddAction);
export default ConnectedAddAction;
