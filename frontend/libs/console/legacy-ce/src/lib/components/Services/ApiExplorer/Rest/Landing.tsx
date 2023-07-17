import React from 'react';

import TopicDescription from '../../Common/Landing/TopicDescription';
import LandingImage from './LandingImage';
import { Button } from '../../../../new-components/Button';
import _push from '../../Data/push';
import { useAppDispatch } from '../../../../storeHooks';
import { Analytics } from '../../../../features/Analytics';

const landingDescription = `REST endpoints allow for the creation of a REST interface to your saved GraphQL queries and mutations.
Endpoints are accessible from /api/rest/* and inherit the authorization and permission structure from your associated GraphQL nodes.
To create a new endpoint simply test your query in GraphiQL then click the REST button on GraphiQL to configure a URL.`;

const Landing = () => {
  const dispatch = useAppDispatch();
  return (
    <div className="pl-md pt-md">
      <div className="flex">
        <h2 className="text-xl font-bold pr-2">REST Endpoints</h2>
        <Analytics name="restified-create-btn-from-landing-page">
          <Button
            mode="primary"
            size="sm"
            onClick={() => dispatch(_push('/api/rest/create'))}
          >
            Create REST
          </Button>
        </Analytics>
      </div>
      <div className="pb-md pr-md">
        Create Rest endpoints on the top of existing GraphQL queries and
        mutations{' '}
      </div>
      <hr className="mb-md" />
      <TopicDescription
        title="What are REST endpoints?"
        imgElement={<LandingImage />}
        imgAlt="REST endpoints"
        description={landingDescription}
        learnMoreHref="https://hasura.io/docs/latest/graphql/core/api-reference/restified.html"
      />
      <hr className="clear-both my-lg" />
    </div>
  );
};

export default Landing;
