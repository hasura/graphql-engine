import React from 'react';
import ReusableTextAreaWithCopy from '../../Layout/ReusableTextAreaWithCopy/ReusableTextAreaWithCopy';
import RemoteSchemaContent from '../../CommonLanding/RemoteSchemaContent';
import EventTriggerContent from '../../CommonLanding/EventTriggerContent';
const styles = require('../Popup.scss');
import PropTypes from 'prop-types';
const ContentMap = {
  "remoteSchema" : <RemoteSchemaContent styles = {styles}/>,
  "eventTrigger" : <EventTriggerContent styles = {styles}/>
}
class PopUp extends React.Component {
  render() {

    const close = require('./cancel.svg');
    const { onClose, title, queryDefinition, footerDescription } = this.props;
    // const queryDefinition = 'query { hello }';
    return (
      <div className={styles.popupWrapper}>
        <div className={styles.wd100}>
          <div className={styles.descriptionText + ' ' + styles.fontWeightBold + ' ' + styles.addPaddBottom + ' ' + styles.commonBorBottom}>
            { title }
          </div>
          <div className={styles.close} onClick={ onClose }>
            <img className={'img-responsive'} src={close} alt={'Close'} />
          </div>
          <div className={styles.arrowLeft}></div>
          {ContentMap[this.props.service]}
          <div className={styles.addPaddLeft + ' text-left ' + styles.addPaddTop}>
            <ReusableTextAreaWithCopy
              copyText={queryDefinition}
              textLanguage={'graphql'}
            />
          </div>
          <div className={styles.listItems}>
            <div className={styles.descriptionText + ' ' + styles.addPaddLeft}>
              { footerDescription }
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
