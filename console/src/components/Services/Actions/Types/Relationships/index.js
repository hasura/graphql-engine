import Relationships from './Main';
import { customTypesSelector } from '../../../../../metadata/selector';

const mapStateToProps = state => {
  return {
    allTypes: customTypesSelector(state),
    ...state.action.types,
  };
};

const connector = connect => connect(mapStateToProps)(Relationships);
export default connector;
