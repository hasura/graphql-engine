import LandingImage from './LandingImage';
import { Flex, Heading } from '@radix-ui/themes';
import { Button, Separator, Text, TopicDescription } from '@hasura/shared/ui';
import { Analytics } from '@hasura/shared/analytics';
import { useNavigate } from 'react-router';

const landingDescription = `REST endpoints allow for the creation of a REST interface to your saved GraphQL queries and mutations.
Endpoints are accessible from /api/rest/* and inherit the authorization and permission structure from your associated GraphQL nodes.
To create a new endpoint simply test your query in GraphiQL then click the REST button on GraphiQL to configure a URL.`;

const Landing = () => {
  const navigate = useNavigate();

  return (
    <div className="pt-4">
      <Flex>
        <Heading size="4">REST Endpoints</Heading>
        <Analytics name="restified-create-btn-from-landing-page">
          <Button
            mode="primary"
            size="sm"
            onClick={() => navigate('/api/rest/create')}
          >
            Create REST
          </Button>
        </Analytics>
      </Flex>
      <div className="pb-4">
        <Text>
          Create Rest endpoints on the top of existing GraphQL queries and
          mutations{' '}
        </Text>
      </div>
      <Separator size="4" className="mb-4" />
      <TopicDescription
        title="What are REST endpoints?"
        imgElement={<LandingImage />}
        imgAlt="REST endpoints"
        description={landingDescription}
        learnMoreHref="https://hasura.io/docs/latest/graphql/core/api-reference/restified.html"
      />
      <Separator size="4" className="my-6" />
    </div>
  );
};

export default Landing;
