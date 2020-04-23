import React from 'react';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import { StyledTooltip } from './Tooltip';

export type ToolTypeGeneratorProps = {
  message: string;
};

export const tooltipGenerator: React.FC<ToolTypeGeneratorProps> = ({
  message,
}) => {
  return <Tooltip id={message}>{message}</Tooltip>;
};

export type ToolTipProps = {
  message: ToolTypeGeneratorProps;
  placement: string;
  children: any;
};

export const ToolTip: React.FC<ToolTipProps> = props => {
  const { message, placement, children } = props;
  return (
    <OverlayTrigger placement={placement} overlay={tooltipGenerator(message)}>
      <StyledTooltip aria-hidden="true" {...props}>
        {children}
      </StyledTooltip>
    </OverlayTrigger>
  );
};
