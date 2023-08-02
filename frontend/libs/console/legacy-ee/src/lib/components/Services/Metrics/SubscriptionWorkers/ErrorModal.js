import { Dialog } from '@hasura/console-legacy-ce';
import styles from '../Metrics.module.scss';

import { decodeError } from './utils';

export const ErrorModal = ({ show, onHide, err }) => {
  const decodedError = { ...decodeError(err) };
  if (!show) return null;

  return (
    <Dialog
      hasBackdrop
      size="xxl"
      id="dateModal"
      onClose={onHide}
      title="Error"
      portal
    >
      <div className={styles.modalWrapper}>
        <div className={styles.modalContainer}>
          <div className={`col-md-12 ${styles.addPaddingBottom}`}>
            <p style={{ padding: '30px 26px 5px 26px' }}>
              Failed loading config:
            </p>
            <div className={styles.boxwrapper}>
              <div className={styles.box + ' ' + styles.errorBox}>
                <code className={styles.queryCode}>
                  <pre style={{ whitespace: 'pre-wrap' }}>
                    {JSON.stringify(decodedError, null, 4)}
                  </pre>
                </code>
              </div>
            </div>
          </div>
        </div>
      </div>
    </Dialog>
  );
};
