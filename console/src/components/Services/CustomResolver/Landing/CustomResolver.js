import React from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';
import { appPrefix, pageTitle } from '../constants';
import globals from '../../../../Globals';
import Button from '../../Layout/Button/Button';
import TopicDescription from '../../CommonLanding/TopicDescription';
import TryItOut from '../../CommonLanding/TryItOut';
class CustomResolver extends React.Component {
  render() {
    const styles = require('../Styles.scss');
    const node = require('./Node.svg');
    const Rectangle = require('./Rectangle.svg');

    // const lightGrayArrow = require('./light-gray-arrow.svg');
    // const darkGrayArrow = require('./dark-gray-arrow.svg');

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
            <div className={styles.display_flex}>
              <h2
                className={`${styles.headerText} ${styles.addPaddRight} ${
                  styles.inline_block
                }`}
              >
                Remote Schemas
              </h2>
              {migrationMode ? (
                <Button
                  data-test="data-create-remote-schemas"
                  color="yellow"
                  size="sm"
                  onClick={e => {
                    e.preventDefault();
                    dispatch(
                      push(`${globals.urlPrefix}${appPrefix}/manage/add`)
                    );
                  }}
                >
                  Add
                </Button>
              ) : null}
            </div>
            <hr />
            <TopicDescription
              title="What are Remote Schemas?"
              imgUrl="https://storage.googleapis.com/hasura-graphql-engine/console/assets/remote_schema.png"
              imgAlt="Remote Schema"
              description="Remote schemas are external GraphQL services which can be merged with Hasura to provide a unified GraphQL API. Think of it like automated schema stitching. All you need to do is build a GraphQL service and then provide its HTTP endpoint to Hasura. Your GraphQL service can be written in any language or framework."
            />
            <hr className={styles.clear_fix} />
            <TryItOut
              service="remoteSchema"
              queryDefinition="query { hello }"
              footerDescription="You just added a remote schema and queried it!"
              glitchLink="https://github.com/hasura/graphql-engine/tree/master/community"
              googleCloudLink="https://github.com/hasura/graphql-engine/tree/master/community"
              MicrosoftAzureLink="https://github.com/hasura/graphql-engine/tree/master/community"
              awsLink="https://github.com/hasura/graphql-engine/tree/master/community"
              adMoreLink="https://github.com/hasura/graphql-engine/tree/master/community"
            />
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
