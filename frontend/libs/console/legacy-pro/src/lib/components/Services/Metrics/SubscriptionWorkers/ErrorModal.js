import React from 'react';
import BootstrapModal from 'react-bootstrap/lib/Modal';

import styles from '../Metrics.scss';

import { decodeError } from './utils';

export const ErrorModal = ({ show, onHide, err }) => {
  const decodedError = { ...decodeError(err) };
  return (
    <BootstrapModal onHide={onHide} show={show} className={styles.modalWrapper}>
      <BootstrapModal.Header className={styles.modalHeader} closeButton>
        <BootstrapModal.Title className={styles.title}>
          Error
        </BootstrapModal.Title>
      </BootstrapModal.Header>
      <BootstrapModal.Body className={styles.modalContainer}>
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
      </BootstrapModal.Body>
    </BootstrapModal>
  );
};
