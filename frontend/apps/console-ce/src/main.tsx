import { StrictMode } from 'react';
import * as ReactDOM from 'react-dom';

import App from './app/App';

ReactDOM.render(
  <StrictMode>
    <App />
  </StrictMode>,
  document.getElementById('root') as HTMLElement
);
