import styles from './console-legacy-pro.module.scss';

import { ConsoleLegacyOss } from '@hasura/console/legacy-oss';

export function ConsoleLegacyPro(props) {
  return (
    <div className={styles['container']}>
      <ConsoleLegacyOss />
      <h1>Welcome to ConsoleLegacyPro!</h1>
    </div>
  );
}
export default ConsoleLegacyPro;
