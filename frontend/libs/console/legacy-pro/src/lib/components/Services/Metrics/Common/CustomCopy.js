import React, { useState } from 'react';
import { CopyToClipboard } from 'react-copy-to-clipboard';

import { EditIcon } from './EditIcon';

const styles = require('../Metrics.scss');
const copyImg = require('../images/copy.svg');

const CustomCopy = ({ label, copy, onEdit }) => {
  const [isCopied, toggle] = useState(false);
  const onCopy = () => {
    toggle(true);
    setTimeout(() => toggle(false), 3000);
  };
  const renderCopyIcon = () => {
    if (isCopied) {
      // To suri modify it to have some kind of tooltip saying copied
      return (
        <div className={styles.copyIcon + ' ' + styles.copiedIcon}>
          <img
            className={styles.copyIcon + ' ' + styles.copiedIcon}
            src={copyImg}
            alt={'Copy icon'}
          />
          <div className={styles.copiedWrapper}>Copied</div>
        </div>
      );
    }
    return <img className={styles.copyIcon} src={copyImg} alt={'Copy icon'} />;
  };
  return (
    <React.Fragment>
      <div className={styles.infoWrapper}>
        <div className={styles.information}>
          {label}:
          <span>
            {onEdit && (
              <EditIcon
                onClick={onEdit}
                className={`${styles.customCopyEdit} ${styles.addPaddingRight}`}
              />
            )}
            <CopyToClipboard text={copy} onCopy={onCopy}>
              {renderCopyIcon()}
            </CopyToClipboard>
          </span>
        </div>
      </div>
      <div className={styles.boxwrapper + ' ' + styles.errorBox}>
        <div className={styles.box}>
          <code className={styles.queryCode}>
            <pre style={{ whitespace: 'pre-wrap' }}>{copy}</pre>
          </code>
        </div>
      </div>
    </React.Fragment>
  );
};

export default CustomCopy;
