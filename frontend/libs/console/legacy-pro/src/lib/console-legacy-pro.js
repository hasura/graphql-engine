import styles from './console-legacy-pro.module.scss';
import { add } from '@hasura/console/legacy-oss';

export function ConsoleLegacyPro(props) {
  return (
    <div className={styles['container']}>
      <h1>Welcome to ConsoleLegacyPro!</h1>
      {add(1, 2)}
    </div>
  );
}
export default ConsoleLegacyPro;
