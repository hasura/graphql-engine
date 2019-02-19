import React from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';

import { appPrefix, pageTitle } from '../constants';
import globals from '../../../../Globals';
import Button from '../../Layout/Button/Button';
import ReusableTextAreaWithCopy from '../../Layout/ReusableTextAreaWithCopy/ReusableTextAreaWithCopy';
class CustomResolver extends React.Component {
  render() {
    const styles = require('../Styles.scss');
    // const landingImage = require('./schema-stitching-color.png');
    // const landingImage = 'https://storage.googleapis.com/hasura-graphql-engine/console/assets/schema-stitching-diagram.png';

    const { dispatch, migrationMode } = this.props;
    const queryDefinition = 'query { hello }';
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
            <div className={styles.subheading_text}>
              Try it out
            </div>
            <p>1.
              <a className={styles.add_mar_left_small} href="https://hasura-sample-remote-schema-4.glitch.me/" target="_blank"><button className={'btn btn-sm ' + styles.yellow_button}>Deploy with Glitch</button></a>
              <span className={styles.add_pad_left}>click to deploy an example GraphQL service to Glitch</span>
            </p>
            <p>
              2. Add the GraphQL service as a remote schema:
            </p>
            <p className={styles.add_pad_left}>
              - Click on the show button in the Glitch console and copy the URL
            </p>
            <p className={styles.add_pad_left}>
              - Add the URL as a remote schema by clicking on the <b>Add</b> button at the top of this page
            </p>
            <p className={styles.add_pad_left}>
              - Set the name as “Sample Remote Schema” and enter the above URL as the <b>GraphQL server URL</b>
            </p>
            <p className={styles.add_pad_left}>
              - click on the <b>Add Remote Schema</b> button - That’s it!
            </p>
            <p>3. Head to the GraphQL tab and try out the following query:</p>
            <ReusableTextAreaWithCopy
              copyText={queryDefinition}
              textLanguage={'graphql'}
            />
            <p className={styles.add_pad_top}>You just added a remote schema and queried it. Yay!</p>
            <hr />
            <div className={styles.subheading_text}>
              Get Started
            </div>
            <p>
              Read the docs for more information
            </p>
            <p>
              Boilerplates
            </p>
            <p>
              Check out the individual READMEs for detailed deployment instructions
            </p>
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
