import React from 'react';
import PropTypes from 'prop-types';

import styles from '../ApiExplorer.scss';

const ApiRequestDetails = () => (
  <div className={styles.apiRequestWrapper + ' ' + styles.apiContentPadd}>
    <div className={styles.apiRequestheader}>{this.props.title}</div>
    <div className={styles.apiRequestContent}>{this.props.description}</div>
  </div>
);

ApiRequestDetails.propTypes = {
  title: PropTypes.string.isRequired,
  description: PropTypes.string.isRequired,
};

export default ApiRequestDetails;
