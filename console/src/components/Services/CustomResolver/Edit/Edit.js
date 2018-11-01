import React from 'react';
import Common from '../Common/Common';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';

const refresh = (
  <Tooltip id="tooltip-cascade">
    If your remote schema has changed, you need to refresh the GraphQL Engine
    metadata to query the modified schema
  </Tooltip>
);
class Edit extends React.Component {
  render() {
    const styles = require('../Styles.scss');
    return (
      <div className={styles.addWrapper}>
        <div className={styles.subheading_text}>my-graphQL-server</div>
        <Common />
        <div className={styles.commonBtn}>
          <button className={styles.yellow_button}>Modify</button>
          <button className={styles.danger_button + ' btn-danger'}>
            Delete
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

export default Edit;
