import React from 'react';

const WhitelistQueriesList = props => {
  const { whitelistQueries } = props;

  const styles = require('../Metadata.scss');

  const getQueryList = () => {
    if (whitelistQueries.length === 0) {
      return <b>No queries whitelisted yet</b>;
    }

    return whitelistQueries.map((query, i) => {
      return (
        <pre key={i} className={styles.add_mar_bottom}>
          {query}
        </pre>
      );
    });
  };

  return (
    <div>
      <h4 className={styles.subheading_text}>Whitelisted Queries</h4>
      <div className={styles.subsection}>{getQueryList()}</div>
    </div>
  );
};

export default WhitelistQueriesList;
