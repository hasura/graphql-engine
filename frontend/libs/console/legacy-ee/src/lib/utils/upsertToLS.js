import {
  persistGraphiQLHeaders,
  getPersistedGraphiQLHeaders,
} from '@hasura/console-legacy-ce';

import globals from '../Globals';

import {
  CONSTANT_HEADERS,
  CLIENT_NAME_HEADER,
  CLIENT_NAME_HEADER_VALUE,
  HASURA_COLLABORATOR_TOKEN,
} from '../constants';
import extendedGlobals from '../Globals';

const adminSecretLabel = `x-hasura-${globals.adminSecretLabel}`;
const personalAccessTokenLabel = globals.patLabel;

const persistFilteredGraphiQLHeaders = headers => {
  const filteredHeaders = headers.filter(header => {
    if (
      extendedGlobals.isAdminSecretSet &&
      extendedGlobals.adminSecret &&
      extendedGlobals.consoleType === 'cloud'
    ) {
      if (header.key.toLowerCase() === HASURA_COLLABORATOR_TOKEN) {
        return false;
      }
    }
    return true;
  });
  persistGraphiQLHeaders(filteredHeaders);
};

const upsertToLS = (key, value) => {
  const commonHead = {
    isActive: true,
    isNewHeader: false,
    isDisabled: true,
  };
  const emptyHeader = {
    key: '',
    value: '',
    isActive: false,
    isNewHeader: true,
    isDisabled: false,
  };
  const getConstantHeaders = () => {
    const constantHeaderKeys = Object.keys(CONSTANT_HEADERS);
    if (constantHeaderKeys.length === 0) {
      return {};
    }
    return constantHeaderKeys.map(c => {
      return {
        key: c,
        value: CONSTANT_HEADERS[c],
        ...commonHead,
      };
    });
  };
  const newHead = [
    ...getConstantHeaders(),
    {
      key: key,
      value: value,
      ...commonHead,
    },
    {
      ...emptyHeader,
    },
  ];
  const HEADER_FROM_LS = getPersistedGraphiQLHeaders();
  if (HEADER_FROM_LS) {
    try {
      let initialHeader = [...HEADER_FROM_LS];
      let isSet = false;
      initialHeader.forEach(i => {
        if (i.key === key) {
          i.value = value;
          isSet = true;
        }
      });
      if (!isSet) {
        initialHeader = [
          {
            key: key,
            value: value,
            ...commonHead,
          },
          ...initialHeader,
        ];
      }

      const nameHeader = initialHeader.filter(
        iH => iH.key === CLIENT_NAME_HEADER
      );
      if (nameHeader.length === 0) {
        initialHeader = [
          {
            key: CLIENT_NAME_HEADER,
            value: CLIENT_NAME_HEADER_VALUE,
            ...commonHead,
          },
          ...initialHeader,
        ];
      }
      /*
       * Remove the conflicting headers
       * like x-hasura-collaborator-token conflicts
       * with admin-secret (this is not applicable for team console)
       */
      if (key.toLowerCase() === adminSecretLabel) {
        initialHeader = initialHeader.filter(i => {
          return ![
            globals.collabLabel,
            globals.ssoLabel,
            globals.patLabel,
          ].includes(i.key);
        });
      } else if (key.toLowerCase() === globals.collabLabel) {
        // The admin secret should not be filtered for team console
        if (extendedGlobals.consoleType !== 'cloud') {
          initialHeader = initialHeader.filter(i => {
            return ![globals.ssoLabel, globals.patLabel].includes(i.key);
          });
        }
      } else if (key.toLowerCase() === globals.patLabel) {
        initialHeader = initialHeader.filter(i => {
          return i.key !== personalAccessTokenLabel;
        });
      } else if (key.toLowerCase() === globals.ssoLabel) {
        initialHeader = initialHeader.filter(i => {
          return ![globals.collabLabel, globals.patLabel].includes(i.key);
        });
      }
      persistFilteredGraphiQLHeaders(initialHeader);
    } catch (e) {
      console.error(e);
      persistFilteredGraphiQLHeaders(newHead);
    }
  } else {
    persistFilteredGraphiQLHeaders(newHead);
  }
};

const removeHeaderFromLS = key => {
  const HEADER_FROM_LS = getPersistedGraphiQLHeaders();
  if (HEADER_FROM_LS) {
    try {
      let initialHeader = [...HEADER_FROM_LS];
      let headerIndex = -1;
      initialHeader.forEach((i, index) => {
        if (i.key === key.toLowerCase() || i.key === key) {
          headerIndex = index;
        }
      });

      if (headerIndex !== -1) {
        initialHeader = [
          ...initialHeader.slice(0, headerIndex),
          ...initialHeader.slice(headerIndex + 1),
        ];
      }
      persistFilteredGraphiQLHeaders(initialHeader);
    } catch (e) {
      console.error(e);
    }
  }
};

export { removeHeaderFromLS };

export default upsertToLS;
