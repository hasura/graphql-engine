import React from 'react';
import { Link } from 'react-router';

import TopicDescription from '../../Common/Landing/TopicDescription';
import LandingImage from './LandingImage';

const landingDescription = `REST endpoints allow for the creation of a REST interface to your saved GraphQL queries and mutations.
Endpoints are accessible from /api/rest/* and inherit the authorization and permission structure from your associated GraphQL nodes. 
To create a new endpoint simply test your query in GraphiQL then click the REST button on GraphiQL to configure a URL.`;

const Landing = () => (
  <div className="pl-md pt-md">
    <div className="flex">
      <h2 className="text-xl font-bold">REST Endpoints</h2>
    </div>
    <div className="pt-md">
      Create endpoints from GraphQL queries using{' '}
      <Link to="/api/api-explorer">GraphiQL</Link>.
    </div>
    <hr className="mb-md" />
    <TopicDescription
      title="What are REST endpoints?"
      imgElement={<LandingImage />}
      imgAlt="REST endpoints"
      description={landingDescription}
      knowMoreHref="https://hasura.io/docs/latest/graphql/core/api-reference/restified.html"
    />
    <hr className="clear-both my-lg" />
  </div>
);

export default Landing;
