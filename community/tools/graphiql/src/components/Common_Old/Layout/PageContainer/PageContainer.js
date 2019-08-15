import React from 'react';
import Helmet from 'react-helmet';

class PageContainer extends React.Component {
  render() {
    const styles = require('../../Common.scss');

    const { helmet, leftContainer, children } = this.props;

    return (
      <div>
        <Helmet title={helmet} />
        <div className={styles.wd20 + ' ' + styles.align_left}>
          {leftContainer}
        </div>
        <div className={styles.wd80}>{children}</div>
      </div>
    );
  }
}

export default PageContainer;
