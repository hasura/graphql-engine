import React, { ComponentProps } from 'react';
import ReloadMetadata from './ReloadMetadata';
import ResetMetadata from './ResetMetadata';
import styles from '../Settings.scss';

type MetadataUpdateSectionProps = ComponentProps<typeof ReloadMetadata>;

export const MetadataUpdateSection: React.FC<MetadataUpdateSectionProps> = props => {
  return (
    <div>
      <div key="meta_data_1" className={styles.intro_note}>
        <h4>Reload metadata</h4>
        <div className={styles.content_width}>
          Refresh Hasura metadata, typically required if you have changed the
          underlying postgres or if you have updated your remote schemas.
        </div>
      </div>

      <div key="meta_data_2">
        <ReloadMetadata {...props} />
      </div>

      <div key="meta_data_3" className={styles.intro_note}>
        <h4>Reset metadata</h4>
        <div className={styles.content_width}>
          Permanently clear GraphQL Engine&apos;s metadata and configure it from
          scratch (tracking relevant tables and relationships). This process is
          not reversible.
        </div>
      </div>

      <div key="meta_data_4">
        <ResetMetadata {...props} />
      </div>
    </div>
  );
};
