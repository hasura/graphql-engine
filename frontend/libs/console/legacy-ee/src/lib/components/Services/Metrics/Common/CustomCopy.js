import React, { useState } from 'react';
import { hasuraToast } from '@hasura/console-legacy-ce';
import { CopyToClipboard } from 'react-copy-to-clipboard';

import { EditIcon } from './EditIcon';

import styles from '../Metrics.module.scss';
import copyImg from '../images/copy.svg';

const CustomCopy = ({
  label,
  copy,
  onEdit,
  displayColon = true,
  displayAcknowledgement = true,
  contentMaxHeight,
}) => {
  const [isCopied, toggle] = useState(false);
  const onCopy = () => {
    toggle(true);
    setTimeout(() => toggle(false), 3000);
  };
  const renderCopyIcon = () => {
    if (isCopied) {
      if (displayAcknowledgement) {
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
      } else {
        hasuraToast({
          type: 'success',
          title: 'Copied!',
        });
      }
    }
    return <img className={styles.copyIcon} src={copyImg} alt={'Copy icon'} />;
  };
  return (
    <React.Fragment>
      <div className={styles.infoWrapper}>
        <div className={styles.information}>
          <span>
            {label}
            {displayColon ? ':' : ''}
          </span>
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
        <div
          className={`p-xs overflow-auto ${styles.box}`}
          style={{
            ...(contentMaxHeight ? { maxHeight: contentMaxHeight } : {}),
          }}
        >
          <code className={styles.queryCode}>
            <pre style={{ whitespace: 'pre-wrap' }}>{copy}</pre>
          </code>
        </div>
      </div>
    </React.Fragment>
  );
};

export default CustomCopy;
