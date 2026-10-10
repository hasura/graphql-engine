import React from 'react';
import { FaCompress, FaExpand } from 'react-icons/fa6';
import { IconButton } from '@hasura/shared/ui';

interface Props extends React.ComponentProps<React.FC> {
  isExpanded: boolean;
  onClick: () => void;
}

const ExpanderButton: React.FC<Props> = ({ isExpanded, onClick }) => (
  <IconButton
    variant="outline"
    title={isExpanded ? 'Collapse row' : 'Expand row'}
    data-test={isExpanded ? 'collapse-event' : 'expand-event'}
    onClick={onClick}
  >
    {isExpanded ? <FaCompress /> : <FaExpand />}
  </IconButton>
);

export default ExpanderButton;
