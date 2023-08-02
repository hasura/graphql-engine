import React from 'react';
import { FaInfoCircle } from 'react-icons/fa';
import { Tooltip } from '../../../new-components/Tooltip';
import styles from './Tooltip.module.scss';

export interface TooltipProps extends React.ComponentProps<'i'> {
  message: string;
  placement?: 'right' | 'left' | 'top' | 'bottom';
  className?: string;
  tooltipStyle?: string;
}

const ToolTip: React.FC<TooltipProps> = ({
  message,
  placement = 'right',
  tooltipStyle = '',
  children,
}) => (
  <Tooltip side={placement} tooltipContentChildren={message}>
    {children || (
      <FaInfoCircle
        className={`cursor-pointer ${styles.tooltipIcon} ${tooltipStyle} text-sm`}
        aria-hidden="true"
      />
    )}
  </Tooltip>
);

export default ToolTip;
