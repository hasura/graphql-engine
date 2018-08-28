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
            Hasura Metadata
          </h2>
          <div className="clearfix" />
        </div>
        <div className={metaDataStyles.intro_note}>
          <div className={metaDataStyles.content_width}>
            Hasura metadata stores information about your tables, relationships,
            and permissions that are used to generate the GraphQL schema and
            API.
            <a
              href="https://docs.hasura.io/1.0/graphql/manual/engine-internals/index.html"
              target="_blank"
              rel="noopener noreferrer"
            >
              {' '}
              Read more
            </a>
            .
          </div>
          <br />
          <div className={metaDataStyles.content_width}>
            You can export/import metadata as JSON.
          </div>
        </div>
        <div className={metaDataStyles.display_inline}>
          <ExportMetadata {...this.props} />
        </div>
        <div className={metaDataStyles.display_inline}>
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
