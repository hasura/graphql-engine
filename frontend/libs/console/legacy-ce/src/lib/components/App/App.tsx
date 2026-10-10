import React from 'react';
import { Outlet } from 'react-router';
import { AlertProvider } from '@hasura/shared/ui';
import { trackCustomEvent } from '@hasura/shared/analytics';
import ProgressBarWrapper from './ProgressBarWrapper';

const App = () => {
  React.useEffect(() => {
    const contentElem = document.getElementById('content');
    const loadingElem = document.getElementById('loading');
    const loadingWrapperElem =
      document.getElementsByClassName('loadingWrapper');

    if (contentElem) {
      contentElem.className = contentElem.className + ' show';
    }

    if (loadingElem) {
      loadingElem.style.display = 'none';
    }

    if (loadingWrapperElem.length) {
      const elem = loadingWrapperElem[0] as HTMLElement;
      elem.style.display = 'none';
    }

    trackCustomEvent({
      location: 'Console',
      action: 'Load',
      object: 'App',
    });
  }, []);

  return (
    <AlertProvider>
      <div>
        <ProgressBarWrapper />
        <div>
          <Outlet />
        </div>
      </div>
    </AlertProvider>
  );
};

export default App;
