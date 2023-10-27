import { browserHistory } from 'react-router';
import { LS_KEYS, setLSItem } from '../../../../utils';

export const openInGraphiQL = (query: string) => {
  if (query) {
    setLSItem(LS_KEYS.graphiqlQuery, query);
  }
  browserHistory.push('/api/api-explorer?mode=rest');
};
