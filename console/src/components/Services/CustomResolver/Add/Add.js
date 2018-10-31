import React from 'react';
import Common from '../Common/Common';

class Add extends React.Component {
  render() {
    const styles = require('../Styles.scss');
    return (
      <div className={styles.addWrapper}>
        <div className={styles.heading_text}>Stitch a new GraphQL schema</div>
        <Common />
      </div>
    );
  }
}

export default Add;
