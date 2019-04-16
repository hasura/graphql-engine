import React from 'react';

import dataErrorMapping from './dataErrorMapping';

const styles = require('../ApiExplorer.scss');

const dataApiErrorMap = {
  'postgres-error': dataErrorMapping['postgres-error'],
  'permission-denied': dataErrorMapping['permission-denied'],
  'not-exists': dataErrorMapping['not-exists'],
  'already-tracked': dataErrorMapping['already-tracked'],
  'access-denied': dataErrorMapping['access-denied'],
  'not-supported': dataErrorMapping['not-supported'],
  'already-exists': dataErrorMapping['already-exists'],
  'invalid-json': dataErrorMapping['invalid-json'],
  'invalid-headers': dataErrorMapping['invalid-headers'],
  'dependency-error': dataErrorMapping['dependency-error'],
  'parse-failed': dataErrorMapping['parse-failed'],
  'already-initialised': dataErrorMapping['already-initialised'],
  'constraint-error': dataErrorMapping['constraint-error'],
  'permission-error': dataErrorMapping['permission-error'],
  'unexpected-payload': dataErrorMapping['unexpected-payload'],
  'invalid-params': dataErrorMapping['invalid-params'],
  unexpected: dataErrorMapping['unexpected'], //eslint-disable-line
  'not-found': dataErrorMapping['not-found'],
};

const dataApiSuggest = response => {
  try {
    const respFunc = dataApiErrorMap[response.response.code];
    if (respFunc) {
      return (
        <div
          className={styles.display_inl}
          dangerouslySetInnerHTML={{ __html: respFunc }}
        />
      );
    }
  } catch (e) {
    return '';
  }
  return '';
};

/* */

export { dataApiErrorMap };

export default {
  data: dataApiSuggest,
};
