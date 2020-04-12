import React from 'react';

import ExportMetadata from './ExportMetadata';
import ImportMetadata from './ImportMetadata';
import ReloadMetadata from './ReloadMetadata';
import ResetMetadata from './ResetMetadata';
import { Heading, TextLink } from '../../../UIKit/atoms';
import styles from '../Settings.scss';

const MetadataOptions = props => {
  const getMetadataImportExportSection = () => {
    return (
      <div>
        <div className={styles.intro_note}>
          <Heading as='h4'>Import/Export metadata</Heading>
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
        <div key='meta_data_1' className={styles.intro_note}>
          <Heading as='h4'>Reload metadata</Heading>
          <div className={styles.content_width}>
            Refresh Hasura metadata, typically required if you have changed the
            underlying postgres or if you have updated your remote schemas.
          </div>
        </div>

        <div key='meta_data_2'>
          <ReloadMetadata {...props} />
        </div>

        <div key='meta_data_3' className={styles.intro_note}>
          <Heading as='h4'>Reset metadata</Heading>
          <div className={styles.content_width}>
            Permanently clear GraphQL Engine's metadata and configure it from
            scratch (tracking relevant tables and relationships). This process
            is not reversible.
          </div>
        </div>

        <div key='meta_data_4'>
          <ResetMetadata {...props} />
        </div>
      </div>
    );
  };

  return (
    <div
      className={`${styles.clear_fix} ${styles.padd_left} ${styles.padd_top} ${styles.metadata_wrapper} container-fluid`}
    >
      <div className={styles.subHeader}>
        <Heading as='h2' pb='0px' fontSize='18px'>
          Hasura Metadata Actions
        </Heading>
        <div className='clearfix' />
        <div className={styles.content_width}>
          Hasura metadata stores information about your tables, relationships,
          permissions, etc. that is used to generate the GraphQL schema and API.
          <TextLink
            href='https://hasura.io/docs/1.0/graphql/manual/how-it-works/metadata-schema.html'
            type='moreInfo'
            ml='sm'
            fontSize='link'
          >
            Read more
          </TextLink>
        </div>
      </div>

      {getMetadataImportExportSection()}

      {getMetadataUpdateSection()}
    </div>
  );
};

const mapStateToProps = state => {
  return {
    ...state.main,
    metadata: state.metadata,
    dataHeaders: { ...state.tables.dataHeaders }
  };
};

const metadataOptsConnector = connect =>
  connect(mapStateToProps)(MetadataOptions);

export default metadataOptsConnector;
