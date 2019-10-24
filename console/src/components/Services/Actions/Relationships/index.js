import Relationships from './Main';

const mapStateToProps = state => {
  return {
    ...state.actions.relationships,
    allActions: state.actions.common.actions,
  };
};

const connector = connect => connect(mapStateToProps)(Relationships);
export default connector;
