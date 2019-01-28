import React from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';

import { appPrefix, pageTitle } from '../constants';
import globals from '../../../../Globals';
import Button from '../../Layout/Button/Button';

class CustomResolver extends React.Component {
  render() {
    const styles = require('../Styles.scss');
    // const landingImage = require('./schema-stitching-color.png');
    // const landingImage = 'https://storage.googleapis.com/hasura-graphql-engine/console/assets/schema-stitching-diagram.png';

    const { dispatch, migrationMode } = this.props;
    return (
      <div
        className={`${styles.padd_left_remove} ${
          styles.resolverWrapper
        } container-fluid ${styles.padd_top}`}
      >
        <div className={styles.padd_left}>
          <Helmet title={`${pageTitle}s | Hasura`} />
          <div>
            <h2 className={`${styles.heading_text} ${styles.inline_block}`}>
              Remote Schemas &nbsp;
            </h2>
            {migrationMode ? (
              <Button
                data-test="data-create-remote-schemas"
                color="yellow"
                size="sm"
                onClick={e => {
                  e.preventDefault();
                  dispatch(push(`${globals.urlPrefix}${appPrefix}/manage/add`));
                }}
              >
                Add
              </Button>
            ) : null}
            <hr />
          </div>
          {/*
            <div className={styles.resolverContent}>
              Add pre-CRUD custom business logic like data validation, etc. or also
              fetch data from another GraphQL server by stitching schemas
            </div>
            <div className={styles.resolverImg}>
              <img src={landingImage} />
            </div>
            <div className={styles.commonBtn}>
              <Link
                className={styles.padd_remove_full}
                to={`${appPrefix}/manage/add`}
              >
                <button className={styles.yellow_button}>
                  Add Remote GraphQL schema
                </button>
              </Link>
            </div>
            <div className={styles.readMore}>
              <a
                href="https://docs.hasura.io/1.0/graphql/manual/schema/custom-logic/index.html"
                target="_blank"
                rel="noopener noreferrer"
              >
                Read more
              </a>
            </div>
          */}
        </div>
      </div>
    );
  }
}

const mapStateToProps = state => {
  return {
    migrationMode: state.main.migrationMode,
  };
};

const landingCustomResolverGen = connect =>
  connect(mapStateToProps)(CustomResolver);

export default landingCustomResolverGen;
