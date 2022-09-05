// import './environments/environment';
import { StrictMode } from 'react';
import * as ReactDOM from 'react-dom';
import { App } from '@hasura/console/legacy-oss';

ReactDOM.render(
  <StrictMode>
    <App />
  </StrictMode>,
  document.getElementById('content') as HTMLElement
);
