import React from 'react';

class EventTriggerContent extends React.Component {
  render() {
    const { styles } = this.props;
    return (
      <div>
        <div className={styles.listItems + ' ' + styles.addPaddTop}>
          <div className={styles.yellowCircle} />
          <div className={styles.descriptionText}>
            <span className={styles.fontWeightBold}>Head to the DATA tab</span>{' '}
            and create a table, say `user`, with columns `id` and `name`
          </div>
        </div>
        <div className={styles.listItems}>
          <div className={styles.yellowCircle} />
          <div className={styles.descriptionText + ' ' + styles.fontWeightBold}>
            Click on “Try it with Glitch”
          </div>
        </div>
        <div className={styles.listItems}>
          <div className={styles.yellowCircle} />
          <div className={styles.descriptionText + ' ' + styles.fontWeightBold}>
            Add the Event Trigger
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
            2. Create an event trigger by clicking on the{' '}
            <span className={styles.fontWeightBold}>Add</span> button at the top
            of this page.
          </div>
        </div>
        <div className={styles.listItems}>
          <div className={styles.descriptionText + ' ' + styles.addPaddLeft}>
            3. Set the name as “test-trigger”. Choose `user` table and select
            all operations. Enter the above URL as{' '}
            <span className={styles.fontWeightBold}>WEBHOOK URL</span>
          </div>
        </div>
        <div className={styles.listItems}>
          <div className={styles.descriptionText + ' ' + styles.addPaddLeft}>
            4. Click on the{' '}
            <span className={styles.fontWeightBold}>Add Event Trigger</span>{' '}
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
export default EventTriggerContent;
