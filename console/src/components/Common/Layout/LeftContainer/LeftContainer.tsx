import React from 'react';
import styles from '../../TableCommon/Table.scss';

class LeftContainer extends React.Component {
  render() {

    const { children } = this.props;

    return (
      <div className={styles.pageSidebar + ' col-xs-12 ' + styles.padd_remove}>
        <div>{children}</div>
      </div>
    );
  }
}

export default LeftContainer;
