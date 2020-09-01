import Modify from './Main';
import { actionsSelector, customTypesSelector } from '../selectors';

const mapStateToProps = state => {
  return {
    ...state.actions.modify,
    allActions: actionsSelector(state),
    allTypes: customTypesSelector(state),
  };
};

const connector = connect => connect(mapStateToProps)(Modify);
export default connector;
