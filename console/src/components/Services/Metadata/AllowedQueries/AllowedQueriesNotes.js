import React from 'react';
import styles from './AllowedQueries.scss';

class AllowedQueriesNotes extends React.Component {
  render() {
    return (
      <div>
        <b>Notes</b>
        <ul className={styles.ul_left_small + ' ' + styles.add_mar_top_small}>
          <li>
            You can add queries to an allowed list which will be the only
            queries that will be allowed to be executed in production mode
          </li>
          <li>
            You can add a query to the allowed list by manually pasting the
            query in the below section
          </li>
        </ul>
      </div>
    );
  }
}

export default AllowedQueriesNotes;
