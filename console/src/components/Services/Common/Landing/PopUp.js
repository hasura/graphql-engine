import React from 'react';
import TextAreaWithCopy from '../../../Common/TextAreaWithCopy/TextAreaWithCopy';
import RemoteSchemaContent from './RemoteSchemaContent';
import EventTriggerContent from './EventTriggerContent';

const styles = require('./Popup.scss');

import PropTypes from 'prop-types';

const ContentMap = {
  remoteSchema: <RemoteSchemaContent styles={styles} />,
  eventTrigger: <EventTriggerContent styles={styles} />,
};
class PopUp extends React.Component {
  render() {
    const close = require('./images/cancel.svg');
    const { onClose, title, queryDefinition, footerDescription } = this.props;
    // const queryDefinition = 'query { hello }';
    const commonPopupStyle = this.props.isAvailable
      ? styles.popupWrapper
      : styles.popupWrapperPos;
    const isAvailableText = this.props.isAvailable ? (
      <div className={styles.arrowLeft} />
    ) : null;
    return (
      <div className={commonPopupStyle}>
        <div className={styles.wd100}>
          <div
            className={
              styles.descriptionText +
              ' ' +
              styles.fontWeightBold +
              ' ' +
              styles.addPaddBottom +
              ' ' +
              styles.commonBorBottom
            }
          >
            {title}
          </div>
          <div className={styles.close} onClick={onClose}>
            <img className={'img-responsive'} src={close} alt={'Close'} />
          </div>
          {isAvailableText}
          {ContentMap[this.props.service]}
          <div
            className={styles.addPaddLeft + ' text-left ' + styles.addPaddTop}
          >
            <TextAreaWithCopy
              copyText={queryDefinition}
              textLanguage={'graphql'}
              id={'copyQueryDefinition'}
            />
          </div>
          <div className={styles.listItems}>
            <div className={styles.descriptionText + ' ' + styles.addPaddLeft}>
              {footerDescription}
            </div>
          </div>
        </div>
      </div>
    );
  }
}
PopUp.propTypes = {
  onClose: PropTypes.func.isRequired,
  title: PropTypes.string.isRequired,
  queryDefinition: PropTypes.string.isRequired,
  footerDescription: PropTypes.string.isRequired,
  service: PropTypes.string.isRequired,
};
export default PopUp;
