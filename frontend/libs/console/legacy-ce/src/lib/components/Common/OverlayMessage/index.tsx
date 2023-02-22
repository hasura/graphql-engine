import React from 'react';
import { Tooltip, TooltipProps } from '../../../new-components/Tooltip';

type Props = {
  children: React.ReactElement;
  message: string;
  placement?: TooltipProps['side'];
};

const OverlayMessage: React.FC<Props> = ({
  message,
  children,
  placement = 'left',
}) =>
  message ? (
    <Tooltip side={placement} tooltipContentChildren={message}>
      {children}
    </Tooltip>
  ) : (
    children
  );

export default OverlayMessage;
