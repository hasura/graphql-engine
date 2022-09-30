import React from 'react';
import { Button } from '@/new-components/Button';

type GridIconButtonProps = {
  icon: React.ReactElement;
  onClick: () => void;
  type: string;
  rowIndex: number;
  action?: 'submit';
};

export const GridIconButton = ({
  icon,
  onClick,
  type = '',
  rowIndex = 0,
  action,
}: GridIconButtonProps) => (
  <Button
    size="sm"
    icon={icon}
    className="mr-1"
    disabled={false}
    onClick={onClick}
    data-test={`row-${type}-button-${rowIndex}`}
    type={action ?? action}
  />
);
