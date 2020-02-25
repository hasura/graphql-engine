import Relationships from './Main';

const mapStateToProps = state => {
  return {
    allTypes: state.types.types,
    ...state.action.types,
  };
};

const connector = connect => connect(mapStateToProps)(Relationships);
export default connector;
