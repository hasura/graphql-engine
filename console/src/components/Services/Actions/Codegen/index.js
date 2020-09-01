import Codegen from './Main';
import { actionsSelector, customTypesSelector } from '../selectors';

const mapStateToProps = state => {
  return {
    ...state.actions.relationships,
    allActions: actionsSelector(state),
    allTypes: customTypesSelector(state),
  };
};

const connector = connect => connect(mapStateToProps)(Codegen);
export default connector;
