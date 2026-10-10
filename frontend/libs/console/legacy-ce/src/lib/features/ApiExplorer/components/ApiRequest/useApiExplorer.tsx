import { useEffect, useState } from 'react';
import { useSearchParams } from 'react-router';
import { hasuraToast } from '@hasura/shared/ui';
import { getGraphQLEndpoint, getHeadersAsJSON } from '../../utils';
import {
  getDefaultGraphiqlHeaders,
  getPersistedGraphiQLHeaders,
  persistGraphiQLHeaders,
  persistGraphiQLMode,
  getGraphiQLQueryFromLocalStorage,
  createTokenInfo,
} from './utils';
import type { DataHeader, GraphiqlMode } from '@hasura/shared/types';
import { jwtDecode } from 'jwt-decode';
import isEqual from 'lodash/isEqual';
import {
  getErrorMessage,
  getLSItem,
  removeLSItem,
  request,
} from '@hasura/shared/utils';
import { useAuthContext } from '@hasura/shared/context';
import { LS_KEYS } from '@hasura/shared/types';

type Props = {
  numberOfTables: number;
};

const FETCH_REMOTE_FILE_ERROR_MESSAGE = 'Failed to fetch remote query file';
// when there are no tables and nothing in the localstorage, show the following comment in the graphiQL
const NO_TABLES_MESSAGE = `# Looks like you do not have any tables.
  # Click on the "Data" tab on top to create tables
  # Try out GraphQL queries here after you create tables
  `;
const FRESH_GRAPHQL_MSG = '# Try out GraphQL queries here\n';

const createGraphiqlHeader = (): DataHeader => ({
  key: '',
  value: '',
  selected: false,
  isDisabled: false,
});

const createAnalyzingTokenState = () => ({
  isAnalyzing: false,
  headerRow: -1,
});

const useApiExplorer = ({ numberOfTables }: Props) => {
  const { getHeaders } = useAuthContext();
  const [searchParams] = useSearchParams();
  const [query, setQuery] = useState('');
  const [mode, setMode] = useState<GraphiqlMode>('graphql');
  const [headers, setHeaders] = useState<DataHeader[]>([]);
  const [objectHeaders, setObjectHeaders] = useState<Record<string, string>>(
    {},
  );
  const [headersInitialized, setHeadersInitialized] = useState(false);
  const [headerFocus, setHeaderFocus] = useState(false);
  const [analyzingToken, setAnalyzingToken] = useState(
    createAnalyzingTokenState(),
  );
  const [tokenInfo, setTokenInfo] = useState(createTokenInfo());
  const endpoint = getGraphQLEndpoint(mode);

  const resetAnalyzingToken = () => {
    setAnalyzingToken(createAnalyzingTokenState());
  };

  const setPersistedQuery = async () => {
    const queryFile = searchParams.get('query_file');
    const localStorageQuery = getGraphiQLQueryFromLocalStorage();
    if (queryFile) {
      return request(queryFile)
        .then((result) => result.text())
        .then((query) => {
          if (query) {
            setQuery(query);
            return;
          }

          return hasuraToast({
            type: 'warning',
            title: FETCH_REMOTE_FILE_ERROR_MESSAGE,
            message: 'The content of remote query file is empty',
          });
        })
        .catch((err) => {
          return hasuraToast({
            type: 'warning',
            title: FETCH_REMOTE_FILE_ERROR_MESSAGE,
            message: getErrorMessage(err),
          });
        });
    }

    if (numberOfTables === 0 && !localStorageQuery) {
      // FIX ME : this message will be shown, whenever there are no tables tracked and nothing in history (LS),
      // there could still be a possibility when there are no tables but remote schemas even then this message will be shown only for the first time
      // after that when the user type something, the LS gets populated and this message will not be shown afterwards
      setQuery(NO_TABLES_MESSAGE);
      return;
    }

    if (localStorageQuery) {
      if (localStorageQuery.includes('do not have')) {
        setQuery(FRESH_GRAPHQL_MSG);
      } else {
        setQuery(localStorageQuery);
      }
    }
  };

  const toggleGraphiqlMode = () => {
    const newMode = mode === 'relay' ? 'graphql' : 'relay';

    persistGraphiQLMode(newMode);
    setMode(newMode);
  };

  const setAndPersistHeaders = (newHeaders: DataHeader[]) => {
    setHeaders(newHeaders);
    persistGraphiQLHeaders(newHeaders);
  };

  const removeRequestHeader = (index: number) => {
    const newHeaders = headers.filter((_, i) => i !== index);
    setAndPersistHeaders(newHeaders);
    diffAndSetObjectHeaders(newHeaders);
  };

  const changeRequestHeader = (header: DataHeader, index: number) => {
    const newHeaders =
      headers.length <= index
        ? [...headers, header]
        : headers.map((h, i) => (i === index ? header : h));
    setAndPersistHeaders(newHeaders);

    if (index === newHeaders.length - 1 && header.key) {
      // Add a new empty header.
      setHeaders([...newHeaders, createGraphiqlHeader()]);
    }
  };

  const analyzeBearerToken = (
    token: string | null | undefined,
    dataHeaderIndex: number,
  ) => {
    if (!token) {
      return;
    }

    setAnalyzingToken({
      isAnalyzing: true,
      headerRow: dataHeaderIndex,
    });

    try {
      const payload = jwtDecode(token);
      const header = jwtDecode(token, { header: true });
      setTokenInfo({
        header,
        payload,
      });
    } catch (_) {
      const message =
        'This JWT seems to be invalid. Please check the token value and try again!';
      setTokenInfo({
        header: null,
        payload: null,
        error: message,
      });
    }
  };

  useEffect(() => {
    setPersistedQuery();

    const graphqlQueryInLS = getLSItem(LS_KEYS.graphiqlQuery);
    if (graphqlQueryInLS && graphqlQueryInLS.indexOf('do not have') !== -1) {
      removeLSItem(LS_KEYS.graphiqlQuery);
    }

    getHeaders().then((authHeaders) => {
      const persistHeaders = getPersistedGraphiQLHeaders(authHeaders);

      const graphiqlHeaders = [
        ...(persistHeaders?.length
          ? persistHeaders
          : getDefaultGraphiqlHeaders()),
      ];

      setObjectHeaders(getHeadersAsJSON(graphiqlHeaders));
      // add an empty placeholder header
      graphiqlHeaders.push({
        key: '',
        value: '',
        selected: true,
        isDisabled: false,
      });

      // persist headers to local storage
      setHeaders(graphiqlHeaders);
      setHeadersInitialized(true);
    });
  }, []);

  const diffAndSetObjectHeaders = (newHeaders: DataHeader[]) => {
    const newObjectHeaders = getHeadersAsJSON(newHeaders);
    if (!isEqual(newObjectHeaders, objectHeaders)) {
      setObjectHeaders(newObjectHeaders);
    }
  };

  const handleHeaderFocus = () => {
    setHeaderFocus(true);
  };

  const handleHeaderUnfocus = () => {
    setHeaderFocus(false);
    diffAndSetObjectHeaders(headers);
  };

  return {
    query,
    setQuery,
    toggleGraphiqlMode,
    endpoint,
    mode,
    headers,
    setHeaders,
    objectHeaders,
    headerFocus,
    handleHeaderFocus,
    removeRequestHeader,
    changeRequestHeader,
    analyzeBearerToken,
    analyzingToken,
    resetAnalyzingToken,
    tokenInfo,
    headersInitialized,
    handleHeaderUnfocus,
  };
};

export default useApiExplorer;
