import React from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';

import { appPrefix, pageTitle } from '../constants';
import globals from '../../../../Globals';
import Button from '../../../Common/Button/Button';
import TopicDescription from '../../Common/Landing/TopicDescription';
import TryItOut from '../../Common/Landing/TryItOut';

class CustomResolver extends React.Component {
  render() {
    const styles = require('../CustomResolver.scss');

    const { dispatch, customResolverList } = this.props;
    const showIntroSection = !customResolverList.resolvers.length;
    const getIntroSection = () => {
      if (!showIntroSection) {
        return null;
      }

      return (
        <div>
          <TopicDescription
            title="What are Remote Schemas?"
            imgUrl={`${globals.assetsPath}/common/img/remote_schema.png`}
            imgAlt="Remote Schema"
            description="Remote schemas are external GraphQL services which can be merged with Hasura to provide a unified GraphQL API. Think of it like automated schema stitching. All you need to do is build a GraphQL service and then provide its HTTP endpoint to Hasura. Your GraphQL service can be written in any language or framework."
          />
          <hr className={styles.clear_fix} />
        </div>
      );
    };

    const getAddBtn = () => {
      let addBtn = null;

      const handleClick = e => {
        e.preventDefault();
        dispatch(push(`${globals.urlPrefix}${appPrefix}/manage/add`));
      };

      addBtn = (
        <Button
          data-test="data-create-remote-schemas"
          color="yellow"
          size="sm"
          className={styles.add_mar_left}
          onClick={handleClick}
        >
          Add
        </Button>
      );

      return addBtn;
    };

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
              <h2 className={`${styles.headerText} ${styles.inline_block}`}>
                Remote Schemas
              </h2>
              {getAddBtn()}
            </div>
            <hr />

            {getIntroSection()}

            <TryItOut
              service="remoteSchema"
              queryDefinition="query { hello }"
              title="Steps to deploy an example GraphQL service to Glitch"
              footerDescription="You just added a remote schema and queried it!"
              glitchLink="https://glitch.com/edit/#!/hasura-sample-remote-schema"
              googleCloudLink="https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/remote-schemas/google-cloud-functions/nodejs"
              MicrosoftAzureLink="https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/remote-schemas/azure-functions/nodejs"
              awsLink="https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/remote-schemas/aws-lambda/nodejs"
              adMoreLink="https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/remote-schemas/"
              isAvailable={showIntroSection}
            />
          </div>
        </div>
      </div>
    );
  }
}

const mapStateToProps = state => {
  return {
    customResolverList: state.customResolverData.listData,
  };
};

const customResolverConnector = connect =>
  connect(mapStateToProps)(CustomResolver);

export default customResolverConnector;
