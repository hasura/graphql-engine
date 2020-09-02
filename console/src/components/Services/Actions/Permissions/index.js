import Permissions from './Main';
import { actionsSelector } from '../selectors';
import { rolesSelector } from '../../../../metadata/selector';

const mapStateToProps = state => {
  return {
    ...state.actions.permissions,
    allRoles: rolesSelector(state),
    allActions: actionsSelector(state),
    readOnlyMode: state.main.readOnlyMode,
  };
};

const connector = connect => connect(mapStateToProps)(Permissions);
export default connector;
