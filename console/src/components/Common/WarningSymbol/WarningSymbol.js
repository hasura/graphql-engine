import React from 'react';

import { Icon, ToolTip } from '../../UIKit/atoms';
import styles from '../Common.scss';

const WarningSymbol = ({
  tooltipText,
  tooltipPlacement = 'right',
  customStyle = null,
}) => {
  return (
    <div className={styles.display_inline}>
      <ToolTip message={tooltipText} placement={tooltipPlacement}>
        <WarningIcon customStyle={customStyle} />
      </ToolTip>
    </div>
  );
};

export const WarningIcon = ({ customStyle }) => {
  return (
    <Icon
      type="warning"
      color="red.primary"
      className={customStyle ? customStyle : ''}
    />
  );
};

export default WarningSymbol;
