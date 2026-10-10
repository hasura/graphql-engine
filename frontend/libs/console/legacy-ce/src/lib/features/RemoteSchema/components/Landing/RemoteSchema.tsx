import { appPrefix, pageTitle } from '../../constants';
import {
  Button,
  Separator,
  TopicDescription,
  TryItOut,
} from '@hasura/shared/ui';
import { useNavigate } from 'react-router';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { Flex, Heading } from '@radix-ui/themes';

const RemoteSchemaLanding = () => {
  useDocumentTitle(`${pageTitle}s | Hasura`);

  const navigate = useNavigate();
  const { readOnlyMode, envVars } = useAppContext();

  const getAddBtn = () => {
    if (readOnlyMode) {
      return null;
    }

    const handleClick = (e) => {
      e.preventDefault();
      navigate(`${appPrefix}/manage/add`);
    };

    return (
      <div className="ml-2">
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
    <div className="m-4">
      <div>
        <Flex align="center" gap="2">
          <Heading size="4">Remote Schemas</Heading>
          {getAddBtn()}
        </Flex>
        <Separator size="4" className="my-4" />

        <TopicDescription
          title="What are Remote Schemas?"
          imgUrl={`${envVars.assetsPath}/common/img/remote_schema.png`}
          imgAlt="Remote Schema"
          description="Remote schemas are external GraphQL services which can be merged with Hasura to provide a unified GraphQL API. Think of it like automated schema stitching. All you need to do is build a GraphQL service and then provide its HTTP endpoint to Hasura. Your GraphQL service can be written in any language or framework."
          learnMoreHref="https://hasura.io/docs/latest/graphql/core/remote-schemas/index.html"
        />
        <Separator size="4" className="my-4" />

        <TryItOut
          service="remoteSchema"
          queryDefinition="query { hello }"
          title="Steps to deploy an example GraphQL service to Glitch"
          footerDescription="You just added a remote schema and queried it!"
          glitchLink="https://glitch.com/edit/#!/hasura-sample-remote-schema"
          googleCloudLink="https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/remote-schemas/google-cloud-functions/nodejs"
          microsoftAzureLink="https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/remote-schemas/azure-functions/nodejs"
          awsLink="https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/remote-schemas/aws-lambda/nodejs"
          adMoreLink="https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/remote-schemas/"
          isAvailable
        />
      </div>
    </div>
  );
};

export default RemoteSchemaLanding;
