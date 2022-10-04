import React, { useEffect } from 'react';

import { StyledBetaAccessForm } from '../styles/StyledChatApp';
import { ExternalLinkIcon } from './ExternalLinkIcon';

export const BetaAccessForm = (props) => {
  useEffect(() => {
    const script = document.createElement('script');
    script.src = 'https://paperform.co/__embed.min.js';
    document.body.appendChild(script);
  }, []);

  return (
    <StyledBetaAccessForm>
      <div className="flex header-div">
        <img
          loading="lazy"
          alt="Hasura"
          src={
            props?.isDarkThemeActive
              ? 'https://graphql-engine-cdn.hasura.io/assets/main-site/logo_primary_light.svg'
              : 'https://graphql-engine-cdn.hasura.io/assets/main-site/logo_primary_dark.svg'
          }
          className="hasura-logo-img"
        />
        <a
          href="https://github.com/hasura/graphql-engine/tree/master/community/sample-apps/streaming-subscriptions-chat"
          target="_blank"
          rel="noreferrer"
        >
          View Source
          <ExternalLinkIcon
            color={props.isDarkThemeActive ? '#fff' : '#2C64F4'}
          />
        </a>
      </div>
      <h2>Streaming Subscriptions</h2>
      <p>
        Hasura now allows you to instantly create a secure API for clients to fetch
        large amounts of data in Postgres as a continuous stream. Subscribe to Hasura Newsletter
        for updates about new features.
      </p>
      <div
        data-paperform-id="hf-streaming-chat-app"
        data-spinner="1"
        className="paperform"
      />
      {/* <form>
        <input placeholder="Your email address" type="email" />
        <button>Get updates</button>
      </form> */}
    </StyledBetaAccessForm>
  );
};
