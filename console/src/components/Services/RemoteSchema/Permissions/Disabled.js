import React from 'react';

const Disabled = () => {
  return (
    <div>
      This feature is disabled.{' '}
      <a
        href="https://docs.hasura.io"
        target="_blank"
        rel="noopener noreferrer"
      >
        Read more <i className={'fa fa-external-link'} aria-hidden="true" />
      </a>
    </div>
  );
};

export default Disabled;
