import React from 'react';
import ExportSchema from './ExportSchema';

import styles from '../Settings.scss';
import { exportSchema } from '../Actions';
import { MetadataImportExportSection } from './MetadataImportExportSection';
import { MetadataUpdateSection } from './MetadataUpdateSection';

const MetadataOptions = props => {
  return (
    <div
      className={`${styles.clear_fix} ${styles.padd_left} ${styles.padd_top} ${styles.metadata_wrapper} container-fluid`}
    >
      <div className={styles.subHeader}>
        <h2 className={styles.headerText}>Hasura Metadata Actions</h2>
      </div>
      <div className={styles.add_mar_top}>
        <div className={styles.content_width}>
          Hasura metadata stores information about your tables, relationships,
          permissions, etc. that is used to generate the GraphQL schema and
          API.&nbsp;
          <a
            href="https://hasura.io/docs/1.0/graphql/manual/how-it-works/metadata-schema.html"
            target="_blank"
            rel="noopener noreferrer"
          >
            <i>(Read more)</i>
          </a>
        </div>

        <MetadataImportExportSection {...props} />
        <ExportSchema exportSchema={props.exportSchema} />

        <MetadataUpdateSection {...props} />
      </div>
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
const mapDispatchToProps = dispatch => {
  return {
    // dispatching plain actions
    exportSchema: () => dispatch(exportSchema),
    dispatch,
  };
};
const metadataOptsConnector = connect =>
  connect(mapStateToProps, mapDispatchToProps)(MetadataOptions);

export default metadataOptsConnector;
