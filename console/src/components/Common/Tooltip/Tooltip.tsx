import React from 'react';
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
}
const ToolTip: React.FC<TooltipProps> = ({
  message,
  placement = 'right',
  children,
}) => (
  <OverlayTrigger placement={placement} overlay={tooltipGen(message)}>
    {children || (
      <i
        className={`fa fa-question-circle ${styles.tooltipIcon}`}
        aria-hidden="true"
      />
    )}
  </OverlayTrigger>
);
export default ToolTip;
