import Manage from './Main';
import { customTypesSelector } from '../../../../../metadata/selector';

const mapStateToProps = state => {
  return {
    allTypes: customTypesSelector(state),
    readOnlyMode: state.main.readOnlyMode,
    ...state.actions.types,
  };
};

const connector = connect => connect(mapStateToProps)(Manage);
export default connector;
