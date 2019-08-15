import React from 'react';

class LeftContainer extends React.Component {
  render() {
    const styles = require('../../TableCommon/Table.scss');

    const { children } = this.props;

    return (
      <div className={styles.pageSidebar + ' col-xs-12 ' + styles.padd_remove}>
        <div>{children}</div>
      </div>
    );
  }
}

export default LeftContainer;
