import globals from '../../../../Globals';
import React from 'react';
import ExportMetadata from './ExportMetadata';
import ImportMetadata from './ImportMetadata';
import ReloadMetadata from './ReloadMetadata';
import ResetMetadata from './ResetMetadata';
import ClearAdminSecret from './ClearAdminSecret';
import { CONSOLE_ADMIN_SECRET } from '../../../AppState';

const MetadataOptions = props => {
  const styles = require('../Metadata.scss');

  const getMetadataImportExportSection = () => {
    return (
      <div>
        <div className={styles.intro_note}>
          <h4>Import/Export</h4>
          <div className={styles.content_width}>
            Get Hasura metadata as JSON.
          </div>
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

  const getMetadataUpdateSection = () => {
    return (
      <div>
        <div key="meta_data_1" className={styles.intro_note}>
          <h4>Reload metadata</h4>
          <div className={styles.content_width}>
            Refresh Hasura metadata, typically required if you have changed the
            underlying postgres.
          </div>
        </div>

        <div key="meta_data_2">
          <ReloadMetadata {...props} />
        </div>

        <div key="meta_data_3" className={styles.intro_note}>
          <h4>Reset Metadata</h4>
          <div className={styles.content_width}>
            Permanently clear GraphQL Engine's metadata and configure it from
            scratch (tracking relevant tables and relationships). This process
            is not reversible.
          </div>
        </div>

        <div key="meta_data_4">
          <ResetMetadata {...props} />
        </div>
      </div>
    );
  };

  const getClearSecretSection = () => {
    let clearSecretSection = null;

    if (window.localStorage[CONSOLE_ADMIN_SECRET]) {
      clearSecretSection = (
        <div>
          <div key="access_key_reset_1" className={styles.intro_note}>
            <h4>Clear {globals.adminSecretLabel} (logout)</h4>

            <div className={styles.content_width}>
              The console caches the {globals.adminSecretLabel} (
              {globals.adminSecretLabel === 'access-key'
                ? 'HASURA_GRAPHQL_ACCESS_KEY'
                : 'HASURA_GRAPHQL_ADMIN_SECRET'}
              ) in the browser. You can clear this cache to force a prompt for
              the {globals.adminSecretLabel} when the console is accessed next
              using this browser.
            </div>
          </div>

          <div key="access_key_reset_2">
            <ClearAdminSecret {...props} />
          </div>
        </div>
      );
    }

    return clearSecretSection;
  };

  return (
    <div
      className={`${styles.clear_fix} ${styles.padd_left} ${styles.padd_top} ${
        styles.metadata_wrapper
      } container-fluid`}
    >
      <div className={styles.subHeader}>
        <h2 className={`${styles.heading_text} ${styles.remove_pad_bottom}`}>
          Hasura Metadata Actions
        </h2>
        <div className="clearfix" />
        <div className={styles.content_width}>
          Hasura metadata stores information about your tables, relationships,
          permissions, etc. that is used to generate the GraphQL schema and
          API.&nbsp;
          <a
            href="https://docs.hasura.io/1.0/graphql/manual/how-it-works/metadata-schema.html"
            target="_blank"
            rel="noopener noreferrer"
          >
            <i>(Read more)</i>
          </a>
        </div>
      </div>

      {getMetadataImportExportSection()}

      {getMetadataUpdateSection()}

      {getClearSecretSection()}
    </div>
  );
};

const mapStateToProps = state => {
  return {
    ...state.main,
    metadata: state.metadata,
    dataHeaders: { ...state.tables.dataHeaders },
  };
};

const metadataOptsConnector = connect =>
  connect(mapStateToProps)(MetadataOptions);

export default metadataOptsConnector;
