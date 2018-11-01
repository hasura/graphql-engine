import React from 'react';
import Common from '../Common/Common';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';

import {
  fetchResolver,
  deleteResolver,
  RESET,
} from '../Add/addResolverReducer';
import { VIEW_RESOLVER } from '../customActions';
import { push } from 'react-router-redux';

const refresh = (
  <Tooltip id="tooltip-cascade">
    If your remote schema has changed, you need to refresh the GraphQL Engine
    metadata to query the modified schema
  </Tooltip>
);
class Edit extends React.Component {
  componentDidMount() {
    const { resolverName } = this.props.params;
    if (!resolverName) {
      this.props.dispatch(push('/custom-resolver'));
    }
    Promise.all([
      this.props.dispatch(fetchResolver(resolverName)),
      this.props.dispatch({ type: VIEW_RESOLVER, data: resolverName }),
    ]);
  }
  componentWillReceiveProps(nextProps) {
    if (nextProps.params.resolverName !== this.props.params.resolverName) {
      Promise.all([
        this.props.dispatch(fetchResolver(nextProps.params.resolverName)),
        this.props.dispatch({
          type: VIEW_RESOLVER,
          data: nextProps.params.resolverName,
        }),
      ]);
    }
  }
  componentWillUnmount() {
    Promise.all([
      this.props.dispatch({ type: RESET }),
      this.props.dispatch({ type: VIEW_RESOLVER, data: '' }),
    ]);
  }
  render() {
    const styles = require('../Styles.scss');
    const { isRequesting, dispatch } = this.props;
    return (
      <div className={styles.addWrapper}>
        <div className={styles.subheading_text}>my-graphQL-server</div>
        <Common {...this.props} />
        <div className={styles.commonBtn}>
          <button className={styles.yellow_button}>Modify</button>
          <button
            className={styles.danger_button + ' btn-danger'}
            onClick={() => dispatch(deleteResolver())}
          >
            {isRequesting ? 'Deleting ...' : 'Delete'}
          </button>
          <a href="#" target="_blank">
            Refresh Schema
          </a>
          <span>
            <OverlayTrigger placement="right" overlay={refresh}>
              <i className="fa fa-question-circle" aria-hidden="true" />
            </OverlayTrigger>
          </span>
        </div>
      </div>
    );
  }
}
const mapStateToProps = state => {
  return {
    ...state.customResolverData.addData,
  };
};

export default connect => connect(mapStateToProps)(Edit);
