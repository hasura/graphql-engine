import React from 'react';
import { FaCompress, FaExpand } from 'react-icons/fa';
import { Button } from '../../../../../new-components/Button';

interface Props extends React.ComponentProps<React.FC> {
  isExpanded: boolean;
}

const ExpanderButton: React.FC<Props> = ({ isExpanded }) => (
  <Button
    size="sm"
    title={isExpanded ? 'Collapse row' : 'Expand row'}
    icon={isExpanded ? <FaCompress /> : <FaExpand />}
    data-test={isExpanded ? 'collapse-event' : 'expand-event'}
  />
);

export default ExpanderButton;
