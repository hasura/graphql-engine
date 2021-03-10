import * as React from 'react';
import { persistHerokuCallbackSearch } from './utils';

const Handler: React.FC = () => {
  React.useEffect(() => {
    if (typeof window !== undefined) {
      // set the value for heroku callback search in local storage
      persistHerokuCallbackSearch(window.location.search);
      window.close();
    }
  }, []);

  return <>Please wait...</>;
};

export default Handler;
