import React, { useState } from 'react';
import { CopyToClipboard } from 'react-copy-to-clipboard';
import { EditIcon } from './EditIcon';
import styles from '../Metrics.module.scss';
import copyImg from '../images/copy.svg';
import { hasuraToast } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

const CustomCopy = ({
  label,
  copy,
  onEdit = null,
  displayColon = true,
  displayAcknowledgement = true,
  contentMaxHeight = '',
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
        <Flex className={styles.information} align="center" gap="1">
          <span>
            {label}
            {displayColon ? ':' : ''}
          </span>
          {onEdit && (
            <EditIcon
              onClick={onEdit}
              className={`${styles.customCopyEdit} ${styles.addPaddingRight}`}
            />
          )}
          <CopyToClipboard text={copy} onCopy={onCopy}>
            {renderCopyIcon()}
          </CopyToClipboard>
        </Flex>
      </div>
      <div className={styles.boxwrapper + ' ' + styles.errorBox}>
        <div
          className={`px-2 overflow-auto ${styles.box}`}
          style={{
            ...(contentMaxHeight ? { maxHeight: contentMaxHeight } : {}),
          }}
        >
          <code className={styles.queryCode}>
            <pre style={{ whiteSpace: 'pre-wrap' }}>{copy}</pre>
          </code>
        </div>
      </div>
    </React.Fragment>
  );
};

export default CustomCopy;
