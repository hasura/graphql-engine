import styles from './console-legacy-oss.module.scss';

/* eslint-disable-next-line */
export interface ConsoleLegacyOssProps {}

export function ConsoleLegacyOss(props: ConsoleLegacyOssProps) {
  return (
    <div className={styles['container']}>
      <h1>Welcome to ConsoleLegacyOss!</h1>
    </div>
  );
}

export default ConsoleLegacyOss;
