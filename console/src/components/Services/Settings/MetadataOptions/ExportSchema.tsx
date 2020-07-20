import React, { useState } from 'react';
import styles from '../Settings.scss';
import Button from '../../../Common/Button/Button';
import { downloadStringAsPlainFile } from '../../../Common/utils/jsUtils';

interface Props {
  exportSchema: () => Promise<string>;
}

const ExportSchema: React.FC<Props> = ({ exportSchema }) => {
  const [loading, setLoading] = useState(false);
  const downloadSchema = () => {
    setLoading(true);
    exportSchema()
      .then(data => {
        downloadStringAsPlainFile('schema.sql', data);
        setLoading(false);
      })
      .catch(e => {
        setLoading(false);
        console.error(e);
      });
  };
  return (
    <div>
      <div className={styles.intro_note}>
        <h4>Export database schema</h4>
        <div className={styles.content_width}>
          Get database schema as SQL file.
        </div>
      </div>

      <div className={styles.display_inline}>
        <Button
          data-test="data-export-metadata"
          className={styles.margin_right}
          size="sm"
          color="white"
          onClick={downloadSchema}
        >
          {loading ? 'Exporting' : 'Export Schema'}
        </Button>
      </div>
    </div>
  );
};

export default ExportSchema;
