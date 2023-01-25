import Codegen from './Main';
import {
  actionsSelector,
  customTypesSelector,
} from '../../../../metadata/selector';

const mapStateToProps = state => {
  return {
    ...state.actions.relationships,
    allActions: actionsSelector(state),
    allTypes: customTypesSelector(state),
  };
};

const connector = connect => connect(mapStateToProps)(Codegen);
export default connector;
