// import './environments/environment';
import { StrictMode } from 'react';
import * as ReactDOM from 'react-dom';
import { Main } from '@hasura/console-legacy-ee';

ReactDOM.render(<Main />, document.getElementById('content') as HTMLElement);
