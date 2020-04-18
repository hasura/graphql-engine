import React from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';

import { appPrefix, pageTitle } from '../constants';
import globals from '../../../../Globals';
import Button from '../../../Common/Button/Button';
import TopicDescription from '../../Common/Landing/TopicDescription';
import TryItOut from '../../Common/Landing/TryItOut';
import { Heading, Box, Flex } from '../../../UIKit/atoms';
import styles from '../RemoteSchema.scss';

const RemoteSchema = ({ dispatch, readOnlyMode }) => {
  const getAddBtn = () => {
    if (readOnlyMode) {
      return null;
    }

    const handleClick = e => {
      e.preventDefault();
      dispatch(push(`${globals.urlPrefix}${appPrefix}/manage/add`));
    };

    return (
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
  };

  return (
    <Box pl="15px" className="container-fluid" pt="20px">
      <Helmet title={`${pageTitle}s | Hasura`} />
      <div>
        <Flex>
          <Heading as="h2" fontSize="h2" display="inline-block">
            Remote Schemas
          </Heading>
          {getAddBtn()}
        </Flex>
        <hr />

        <TopicDescription
          title="What are Remote Schemas?"
          imgUrl={`${globals.assetsPath}/common/img/remote_schema.png`}
          imgAlt="Remote Schema"
          description="Remote schemas are external GraphQL services which can be merged with Hasura to provide a unified GraphQL API. Think of it like automated schema stitching. All you need to do is build a GraphQL service and then provide its HTTP endpoint to Hasura. Your GraphQL service can be written in any language or framework."
        />
        <hr className={styles.clear_fix} />

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
    </Box>
  );
};

const mapStateToProps = state => {
  return {
    remoteSchemaList: state.remoteSchemas.listData,
    readOnlyMode: state.main.readOnlyMode,
  };
};

const remoteSchemaConnector = connect => connect(mapStateToProps)(RemoteSchema);

export default remoteSchemaConnector;
