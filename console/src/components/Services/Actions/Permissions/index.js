import Permissions from './Main';
import { actionsSelector } from '../selectors';

const mapStateToProps = state => {
  return {
    ...state.actions.permissions,
    allRoles: state.tables.allRoles,
    allActions: actionsSelector(state),
    readOnlyMode: state.main.readOnlyMode,
  };
};

const connector = connect => connect(mapStateToProps)(Permissions);
export default connector;
