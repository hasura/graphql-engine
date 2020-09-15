import React, { ComponentProps } from 'react';
import ExportMetadata from './ExportMetadata';
import ImportMetadata from './ImportMetadata';
import styles from '../Settings.scss';

type MetadataImportExportSectionProps = ComponentProps<typeof ExportMetadata>;

export const MetadataImportExportSection: React.FC<MetadataImportExportSectionProps> = props => {
  return (
    <div>
      <div className={styles.intro_note}>
        <h4>Import/Export metadata</h4>
        <div className={styles.content_width}>Get Hasura metadata as JSON.</div>
      </div>

      <div className={styles.display_inline}>
        <ExportMetadata {...props} />
      </div>

      <div className={styles.display_inline}>
        <ImportMetadata {...props} />
      </div>
    </div>
  );
};
