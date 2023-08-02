import React from 'react';
import { Button } from '../../../../../new-components/Button';

type Props = {
  onToggle: () => void;
  text: React.ReactElement;
};

export const SelectPermissionSectionHeader: React.FC<Props> = ({
  text,
  onToggle,
}) => (
  <div className="flex items-center">
    <span className="mr-sm ">{text}</span>
    <Button size="sm" onClick={onToggle} data-test="toggle-all-col-btn">
      <div className="font-semibold">Toggle All</div>
    </Button>
  </div>
);
