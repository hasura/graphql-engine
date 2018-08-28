import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Helmet from 'react-helmet';

import ExportMetadata from './ExportMetadata';
import ImportMetadata from './ImportMetadata';

class Metadata extends Component {
  render() {
    const styles = require('../TableCommon/Table.scss');
    const metaDataStyles = require('./Metadata.scss');
    return (
      <div
        className={`${styles.main_wrapper} ${styles.padd_left} ${
          styles.padd_top
        } ${metaDataStyles.metadata_wrapper}`}
      >
        <Helmet title="Manage GraphQL Engine Metadata | Hasura" />
        <div className={styles.subHeader}>
          <h2 className={`${styles.heading_text} ${styles.remove_pad_bottom}`}>
            Manage GraphQL Engine Metadata
          </h2>
          <div className="clearfix" />
        </div>
        <hr />
        <div className={metaDataStyles.intro_note}>
          <h4 className={metaDataStyles.intro_note_heading}>
            What is GraphQL engine metadata?
          </h4>
          <div className={metaDataStyles.content_width}>
            Hasura GraphQL engine uses a set of internal tables to manage the
            state of the database and the GraphQL schema. It uses the data in
            these tables to generate the GraphQL API which then can be accessed
            from different clients. Checkout{' '}
            <a
              href="https://docs.hasura.io/1.0/graphql/manual/engine-internals/index.html"
              target="_blank"
              rel="noopener noreferrer"
            >
              docs
            </a>{' '}
            for more info.
          </div>
        </div>
        <hr />
        <h4 className={metaDataStyles.margin_bottom_header}>Export metadata</h4>
        <div className={metaDataStyles.display_inline}>
          <p className={metaDataStyles.content_width}>
            Download GraphQL engine's metadata as a json file. This file will
            come in handy in future while migrating your GraphQL server to a
            different environment or troubleshooting GraphQL engine in case of
            any unexpected errors.
          </p>
          <ExportMetadata {...this.props} />
        </div>
        <hr />
        <h4 className={metaDataStyles.margin_bottom_header}>Import metadata</h4>
        <div>
          <p className={metaDataStyles.content_width}>
            Update GraphQL engine's metadata using a valid metadata file.
          </p>
          <ImportMetadata {...this.props} />
        </div>
      </div>
    );
  }
}

Metadata.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

const metadataConnector = connect => connect()(Metadata);
export default metadataConnector;
