import Relationships from './Main';
import { customTypesSelector } from '../../selectors';

const mapStateToProps = state => {
  return {
    allTypes: customTypesSelector(state),
    ...state.action.types,
  };
};

const connector = connect => connect(mapStateToProps)(Relationships);
export default connector;
