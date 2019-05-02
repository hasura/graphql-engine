import React from 'react';

class WhitelistNotes extends React.Component {
  render() {
    const styles = require('./WhitelistQueries.scss');

    return (
      <div>
        <b>Notes</b>
        <ul className={styles.ul_left_small + ' ' + styles.add_mar_top_small}>
          <li>
            You can whitelist queries which will be the only queries that will
            be allowed to be executed in production mode
          </li>
          <li>
            You can add a query to the whitelist by:
            <ul className={styles.ul_left_small}>
              <li>Manually pasting the query in the below section</li>
              <li>
                Uploading a <i>.graphql</i> file containing the query
              </li>
            </ul>
          </li>
        </ul>
      </div>
    );
  }
}

export default WhitelistNotes;
