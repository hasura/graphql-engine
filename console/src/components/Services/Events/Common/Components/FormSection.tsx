import React from 'react';
import Tooltip from '../../../../Common/Tooltip/Tooltip';

interface Props extends React.ComponentProps<React.FC> {
  heading: string;
  id: string;
  tooltip?: string;
}

const FormSection: React.FC<Props> = ({ children, id, tooltip, heading }) => {
  return (
    <div className="mb-md">
      <h2 className="text-lg font-bold pb-sm">
        {heading}
        {tooltip && <Tooltip id={id} message={tooltip} className="ml-xs" />}
      </h2>
      {children}
      <hr className="my-md" />
    </div>
  );
};

export default FormSection;
