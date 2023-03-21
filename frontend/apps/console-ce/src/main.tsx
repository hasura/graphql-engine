// import './environments/environment';
import * as ReactDOM from 'react-dom';
import { ConsoleCeApp } from '@hasura/console-legacy-ce';

ReactDOM.render(
  <ConsoleCeApp />,
  document.getElementById('content') as HTMLElement
);
