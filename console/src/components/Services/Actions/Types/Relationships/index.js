import Relationships from './Main';
import { connect } from 'react-redux';

const mapStateToProps = state => {
  return {
    allTypes: state.types.types,
    ...state.action.types,
  };
};

const ConnectedRelationships = connect(mapStateToProps)(Relationships);
export default ConnectedRelationships;
