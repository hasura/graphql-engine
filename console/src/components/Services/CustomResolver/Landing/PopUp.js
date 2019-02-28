import React from 'react';
import ReusableTextAreaWithCopy from '../../Layout/ReusableTextAreaWithCopy/ReusableTextAreaWithCopy';

class PopUp extends React.Component {
  render() {
    const styles = require('../Popup.scss');
    const close = require('./cancel.svg');
    const { dispatch, migrationMode } = this.props;
    const queryDefinition = 'query { hello }';
    return (
      <div className={styles.popupWrapper}>
        <div className={styles.wd100}>
          <div className={styles.descriptionText + ' ' + styles.fontWeightBold + ' ' + styles.addPaddBottom + ' ' + styles.commonBorBottom}>
            Steps to deploy an example GraphQL service to Glitch
          </div>
          <div className={styles.close}>
            <img className={'img-responsive'} src={close} alt={'Close'} />
          </div>
          <div className={styles.listItems + ' ' + styles.addPaddTop}>
            <div className={styles.yellowCircle}>
            </div>
            <div className={styles.descriptionText + ' ' + styles.fontWeightBold}>
              Steps to deploy an example GraphQL service to Glitch
            </div>
          </div>
          <div className={styles.listItems}>
            <div className={styles.yellowCircle}>
            </div>
            <div className={styles.descriptionText + ' ' + styles.fontWeightBold}>
              Add the GraphQL service as a Remote Schema:
            </div>
          </div>
          <div className={styles.listItems}>
            <div className={styles.descriptionText + ' ' + styles.addPaddLeft}>
              1. Click on the <span className={styles.fontWeightBold}>SHOW</span> button in the Glitch console and copy the URL.
            </div>
          </div>
          <div className={styles.listItems}>
            <div className={styles.descriptionText + ' ' + styles.addPaddLeft}>
              2. Create a remote schema by clicking the <span className={styles.fontWeightBold}>Add remote schema</span> button at the top of this page
            </div>
          </div>
          <div className={styles.listItems}>
            <div className={styles.descriptionText + ' ' + styles.addPaddLeft}>
              3. Set the name as “Sample Remote Schema” and enter the above URL as the GraphQL server URL
            </div>
          </div>
          <div className={styles.listItems}>
            <div className={styles.descriptionText + ' ' + styles.addPaddLeft}>
              4. Click on the <span className={styles.fontWeightBold}>Create Remote Schema</span> button - That’s it!
            </div>
          </div>
          <div className={styles.listItems}>
            <div className={styles.yellowCircle}>
            </div>
            <div className={styles.descriptionText + ' ' + styles.fontWeightBold}>
              Head to the GraphiQL tab and try out the following query:
            </div>
          </div>
          <div className={styles.addPaddLeft + ' text-left ' + styles.addPaddTop}>
            <ReusableTextAreaWithCopy
              copyText={queryDefinition}
              textLanguage={'graphql'}
            />
          </div>
          <div className={styles.listItems}>
            <div className={styles.descriptionText + ' ' + styles.addPaddLeft}>
              You just added a remote schema and queried it!
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default PopUp;
