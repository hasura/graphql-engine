import globals from '../../../../Globals';
import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Helmet from 'react-helmet';

import ExportMetadata from './ExportMetadata';
import ImportMetadata from './ImportMetadata';
import ReloadMetadata from './ReloadMetadata';
import ResetMetadata from './ResetMetadata';
import ClearAdminSecret from './ClearAdminSecret';

import semverCheck from '../../../../helpers/semver';
import { CONSOLE_ADMIN_SECRET } from '../../../AppState';

class Metadata extends Component {
  constructor() {
    super();
    this.state = {
      showMetadata: false,
    };
  }
  componentDidMount() {
    if (this.props.serverVersion) {
      this.checkSemVer(this.props.serverVersion);
    }
  }
  componentWillReceiveProps(nextProps) {
    if (nextProps.serverVersion !== this.props.serverVersion) {
      this.checkSemVer(nextProps.serverVersion);
    }
  }
  checkSemVer(version) {
    try {
      const showMetadata = semverCheck('metadataReload', version);
      if (showMetadata) {
        this.updateMetadataState(true);
      } else {
        this.updateMetadataState(false);
      }
    } catch (e) {
      this.updateMetadataState(false);
      console.error(e);
    }
  }
  updateMetadataState(displayReloadMetadata) {
    this.setState({
      showMetadata: displayReloadMetadata,
    });
  }
  render() {
    const styles = require('../../../Common/TableCommon/Table.scss');
    const metaDataStyles = require('./Metadata.scss');

    const getMetadataImportExportSection = () => {
      return (
        <div>
          <div className={metaDataStyles.intro_note}>
            <h4>Import/Export</h4>
            <div className={metaDataStyles.content_width}>
              Get Hasura metadata as JSON.
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
    };

    const getMetadataUpdateSection = () => {
      let updateSection = null;

      if (this.state.showMetadata) {
        updateSection = (
          <div>
            <div key="meta_data_1" className={metaDataStyles.intro_note}>
              <h4>Reload metadata</h4>
              <div className={metaDataStyles.content_width}>
                Refresh Hasura metadata, typically required if you have changed
                the underlying postgres.
              </div>
            </div>

            <div key="meta_data_2">
              <ReloadMetadata {...this.props} />
            </div>

            <div key="meta_data_3" className={metaDataStyles.intro_note}>
              <h4>Reset Metadata</h4>
              <div className={metaDataStyles.content_width}>
                Permanently clear GraphQL Engine's metadata and configure it
                from scratch (tracking relevant tables and relationships). This
                process is not reversible.
              </div>
            </div>

            <div key="meta_data_4">
              <ResetMetadata {...this.props} />
            </div>
          </div>
        );
      }

      return updateSection;
    };

    const getClearSecretSection = () => {
      let clearSecretSection = null;

      if (window.localStorage[CONSOLE_ADMIN_SECRET]) {
        clearSecretSection = (
          <div>
            <div key="access_key_reset_1" className={metaDataStyles.intro_note}>
              <h4>Clear {globals.adminSecretLabel} (logout)</h4>

              <div className={metaDataStyles.content_width}>
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
              <ClearAdminSecret {...this.props} />
            </div>
          </div>
        );
      }

      return clearSecretSection;
    };

    return (
      <div
        className={`${styles.clear_fix} ${styles.padd_left} ${
          styles.padd_top
        } ${metaDataStyles.metadata_wrapper} container-fluid`}
      >
        <Helmet title="Manage GraphQL Engine Metadata | Hasura" />
        <div className={styles.subHeader}>
          <h2 className={`${styles.heading_text} ${styles.remove_pad_bottom}`}>
            Hasura Metadata
          </h2>
          <div className="clearfix" />
          <div className={metaDataStyles.content_width}>
            Hasura metadata stores information about your tables, relationships,
            permissions, etc. that is used to generate the GraphQL schema and
            API.{' '}
            <a
              href="https://docs.hasura.io/1.0/graphql/manual/engine-internals/index.html"
              target="_blank"
              rel="noopener noreferrer"
            >
              Read more
            </a>
            .
          </div>
        </div>
        {getMetadataImportExportSection()}

        {getMetadataUpdateSection()}

        {getClearSecretSection()}
      </div>
    );
  }
}

Metadata.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

const mapStateToProps = state => {
  return {
    ...state.main,
    dataHeaders: { ...state.tables.dataHeaders },
  };
};

const metadataConnector = connect => connect(mapStateToProps)(Metadata);
export default metadataConnector;
