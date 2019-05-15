import React, { Component } from 'react';

class BreadCrumb extends Component {
  render() {
    const styles = require('./BreadCrumb.scss');
    return <div className={styles.breadcrumbWrapper}>{this.props.data}</div>;
  }
}

BreadCrumb.propTypes = {
  data: React.PropTypes.string.isRequired,
};

export default BreadCrumb;
