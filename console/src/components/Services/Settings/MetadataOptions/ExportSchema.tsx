import React from 'react';
import styles from '../Settings.scss';
import Button from '../../../Common/Button/Button';

const ExportSchema = props => {
  return (
    <div>
      <div className={styles.intro_note}>
        <h4>Export schema</h4>
        <div className={styles.content_width}>Get database schema as JSON.</div>
      </div>

      <div className={styles.display_inline}>
        <Button
          data-test="data-export-metadata"
          className={styles.margin_right}
          size="sm"
          color="white"
        >
          Export Schema
        </Button>
      </div>
    </div>
  );
};

export default ExportSchema;
