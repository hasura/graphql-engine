import React from 'react';

class RemoteSchemaContent extends React.Component {
  render() {
    const { styles } = this.props;
    return (
      <div>
        <div className={styles.listItems + ' ' + styles.addPaddTop}>
          <div className={styles.yellowCircle} />
          <div className={styles.descriptionText + ' ' + styles.fontWeightBold}>
            Click on “Try it with Glitch”
          </div>
        </div>
        <div className={styles.listItems}>
          <div className={styles.yellowCircle} />
          <div className={styles.descriptionText + ' ' + styles.fontWeightBold}>
            Add the GraphQL service as a Remote Schema:
          </div>
        </div>
        <div className={styles.listItems}>
          <div className={styles.descriptionText + ' ' + styles.addPaddLeft}>
            1. Click on the <span className={styles.fontWeightBold}>SHOW</span>{' '}
            button in the Glitch console and copy the URL.
          </div>
        </div>
        <div className={styles.listItems}>
          <div className={styles.descriptionText + ' ' + styles.addPaddLeft}>
            2. Create a remote schema by clicking the{' '}
            <span className={styles.fontWeightBold}>Add</span> button at the top
            of this page.
          </div>
        </div>
        <div className={styles.listItems}>
          <div className={styles.descriptionText + ' ' + styles.addPaddLeft}>
            3. Set the name as “Test Schema” and enter the above URL as the{' '}
            <span className={styles.fontWeightBold}>GraphQL server URL</span>.
          </div>
        </div>
        <div className={styles.listItems}>
          <div className={styles.descriptionText + ' ' + styles.addPaddLeft}>
            4. Click on the{' '}
            <span className={styles.fontWeightBold}>Add Remote Schema</span>{' '}
            button - That’s it!
          </div>
        </div>
        <div className={styles.listItems}>
          <div className={styles.yellowCircle} />
          <div className={styles.descriptionText + ' ' + styles.fontWeightBold}>
            Head to the GraphiQL tab and try out the following query:
          </div>
        </div>
      </div>
    );
  }
}
export default RemoteSchemaContent;
