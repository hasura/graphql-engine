// import './environments/environment';
import { StrictMode } from 'react';
import * as ReactDOM from 'react-dom';
import { Main } from '@hasura/console/legacy-pro';

ReactDOM.render(
  <StrictMode>
    <Main />
  </StrictMode>,
  document.getElementById('content') as HTMLElement
);
