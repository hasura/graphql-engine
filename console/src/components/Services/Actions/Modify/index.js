import Modify from './Main';
import {
  actionsSelector,
  customTypesSelector,
} from '../../../../metadata/selector';

const mapStateToProps = state => {
  return {
    ...state.actions.modify,
    allActions: actionsSelector(state),
    allTypes: customTypesSelector(state),
    readOnlyMode: state.main.readOnlyMode,
  };
};

const connector = connect => connect(mapStateToProps)(Modify);
export default connector;
