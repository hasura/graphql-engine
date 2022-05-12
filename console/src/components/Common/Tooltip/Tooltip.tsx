import React from 'react';
import { FaInfoCircle } from 'react-icons/fa';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import styles from './Tooltip.scss';

const tooltipGen = (message: string) => {
  return <Tooltip id={message}>{message}</Tooltip>;
};
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
  <OverlayTrigger placement={placement} overlay={tooltipGen(message)}>
    {children || (
      <FaInfoCircle
        className={`cursor-pointer ${styles.tooltipIcon} ${tooltipStyle} text-sm`}
        aria-hidden="true"
      />
    )}
  </OverlayTrigger>
);

export default ToolTip;
