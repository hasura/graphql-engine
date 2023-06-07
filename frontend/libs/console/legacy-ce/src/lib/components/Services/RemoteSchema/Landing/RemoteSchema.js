import React from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';

import { appPrefix, pageTitle } from '../constants';
import globals from '../../../../Globals';
import { Button } from '../../../../new-components/Button';
import TopicDescription from '../../Common/Landing/TopicDescription';
import TryItOut from '../../Common/Landing/TryItOut';

class RemoteSchema extends React.Component {
  render() {
    const { dispatch, readOnlyMode } = this.props;

    const getAddBtn = () => {
      if (readOnlyMode) {
        return null;
      }

      const handleClick = e => {
        e.preventDefault();
        dispatch(push(`${globals.urlPrefix}${appPrefix}/manage/add`));
      };

      return (
        <div className="ml-sm">
          <Button
            data-testid="data-create-remote-schemas"
            mode="primary"
            onClick={handleClick}
          >
            Add
          </Button>
        </div>
      );
    };

    return (
      <div className="pl-0 pt-0 bootstrap-jail">
        <div className="m-sm">
          <Helmet title={`${pageTitle}s | Hasura`} />
          <div>
            <div className="flex">
              <h2 className="text-xl font-bold mr-md">Remote Schemas</h2>
              {getAddBtn()}
            </div>
            <hr className="my-md" />

            <TopicDescription
              title="What are Remote Schemas?"
              imgUrl={`${globals.assetsPath}/common/img/remote_schema.png`}
              imgAlt="Remote Schema"
              description="Remote schemas are external GraphQL services which can be merged with Hasura to provide a unified GraphQL API. Think of it like automated schema stitching. All you need to do is build a GraphQL service and then provide its HTTP endpoint to Hasura. Your GraphQL service can be written in any language or framework."
              learnMoreHref="https://hasura.io/docs/latest/graphql/core/remote-schemas/index.html"
            />
            <hr className={`clear-both my-lg`} />

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
              isAvailable
            />
          </div>
        </div>
      </div>
    );
  }
}

const mapStateToProps = state => {
  return {
    readOnlyMode: state.main.readOnlyMode,
  };
};

const remoteSchemaConnector = connect => connect(mapStateToProps)(RemoteSchema);

export default remoteSchemaConnector;
