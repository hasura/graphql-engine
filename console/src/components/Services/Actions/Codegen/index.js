import Codegen from './Main';

const mapStateToProps = state => {
  return {
    ...state.actions.relationships,
    allActions: state.actions.common.actions,
    allTypes: state.types.types,
  };
};

const connector = connect => connect(mapStateToProps)(Codegen);
export default connector;
