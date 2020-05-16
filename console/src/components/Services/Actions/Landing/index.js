import Landing from './Main';
import { connect } from 'react-redux';

const mapStateToProps = state => {
  return {
    ...state.actions.common,
  };
};

const ConnectedLanding = connect(mapStateToProps)(Landing);
export default ConnectedLanding;
