// eslint-disable-next-line @typescript-eslint/no-unused-vars
import styles from './App.module.css';
import NxWelcome from './nx-welcome';
import { ConsoleLegacyOss } from '@hasura/console/legacy-oss';

export function App() {
  return (
    <>
      <ConsoleLegacyOss />
      <NxWelcome title="console-ce" />
      <div />
    </>
  );
}

export default App;
