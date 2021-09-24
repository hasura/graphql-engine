import React from 'react';
import { Link } from 'react-router';

import TopicDescription from '../../Common/Landing/TopicDescription';
import LandingImage from './LandingImage';

import styles from './RESTStyles.scss';

const landingDescription = `REST endpoints allow for the creation of a REST interface to your saved GraphQL queries and mutations.
Endpoints are accessible from /api/rest/* and inherit the authorization and permission structure from your associated GraphQL nodes. 
To create a new endpoint simply test your query in GraphiQL then click the REST button on GraphiQL to configure a URL.`;

const Landing = () => (
  <div
    className={`container-fluid ${styles.rest_add_padding_left} ${styles.padd_top}`}
  >
    <div className={`${styles.display_flex} ${styles.marginBottom}`}>
      <h2
        className={`${styles.headerText} ${styles.display_inline} ${styles.margin_bottom}`}
      >
        REST Endpoints
      </h2>
    </div>
    <div className={`${styles.subHeader} ${styles.padd_top}`}>
      Create endpoints from GraphQL queries using{' '}
      <Link to="/api/api-explorer">GraphiQL</Link>.
    </div>
    <hr className="my-md" />
    <TopicDescription
      title="What are REST endpoints?"
      imgElement={<LandingImage />}
      imgAlt="REST endpoints"
      description={landingDescription}
      knowMoreHref="https://hasura.io/docs/latest/graphql/core/api-reference/restified.html"
    />
    <hr className={`${styles.clear_fix} my-lg`} />
  </div>
);

export default Landing;
