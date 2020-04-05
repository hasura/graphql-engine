import React from 'react';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';

import { Icon } from '../Icon';
import { StyledTooltip } from './Tooltip';

const tooltipGenerator = message => {
  return <Tooltip id={message}>{message}</Tooltip>;
};

const InfoIcon = <Icon type="questionCircle" size={12} pointer />;

export const ToolTip = props => {
  const { message, placement, children } = props;

  return (
    <OverlayTrigger placement={placement} overlay={tooltipGenerator(message)}>
      <StyledTooltip aria-hidden="true" {...props}>
        {children}
      </StyledTooltip>
    </OverlayTrigger>
  );
};

ToolTip.defaultProps = {
  placement: 'right',
  children: InfoIcon,
};
