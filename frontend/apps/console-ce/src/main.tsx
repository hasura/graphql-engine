// import './environments/environment';
import { StrictMode } from 'react';
import * as ReactDOM from 'react-dom';
import { App } from '@hasura/console-legacy-ce';

ReactDOM.render(<App />, document.getElementById('content') as HTMLElement);
