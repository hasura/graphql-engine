import React from 'react';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';

type Props = {
  children: React.ReactElement;
  message: string;
  placement?: string;
};

const OverlayMessage: React.FC<Props> = ({
  message,
  children,
  placement = 'left',
}) =>
  message ? (
    <OverlayTrigger
      placement={placement}
      overlay={<Tooltip id={message}>{message}</Tooltip>}
    >
      {children}
    </OverlayTrigger>
  ) : (
    children
  );

export default OverlayMessage;
