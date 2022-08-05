// eslint-disable-next-line @typescript-eslint/no-unused-vars
import styles from './App.module.css';
import NxWelcome from './nx-welcome';
import { ConsoleLegacyPro } from '@hasura/console/legacy-pro';

export function App() {
  return (
    <>
      <ConsoleLegacyPro />
      <NxWelcome title="console-pro" />
      <div />
    </>
  );
}

export default App;
