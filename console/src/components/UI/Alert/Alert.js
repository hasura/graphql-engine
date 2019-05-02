import React from 'react';
import styles from './Alert.scss';
const closeIcon = require('./images/x-circle.svg');
const success = require('./images/check.svg');
const info = require('./images/info.svg');
const warning = require('./images/warning.svg');
const error = require('./images/error.svg');
const Alert = props => {
  const { children, type } = props;

  let alertTypeStyle;
  let alertText;
  let alertIcon;
  switch (type) {
      case 'alertSuccess':
        alertTypeStyle = styles.alertSuccess;
        alertText = 'Success';
        alertIcon = success;
      break;
      case 'alertInfo':
        alertTypeStyle = styles.alertInfo;
        alertText = 'Information';
        alertIcon = info;
      break;
      case 'alertWarning':
        alertTypeStyle = styles.alertWarning;
        alertText = 'Warning';
        alertIcon = warning;
      break;
      case 'alertDanger':
        alertTypeStyle = styles.alertDanger;
        alertText = 'Error';
        alertIcon = error;
      break;
    default:
      alertTypeStyle = styles.alertSuccess;
      alertText = 'Success';
      alertIcon = success;
      break;
  }
  return (
    <div className={styles.alert + ' ' + alertTypeStyle}>
      <img className={styles.alertImg} src={alertIcon} alt={'alert icon'} />
      <span>{alertText}</span>
      {children}
      <img className={styles.close} src={closeIcon} alt={'Close icon'} />
    </div>
  );
};

export default Alert;
