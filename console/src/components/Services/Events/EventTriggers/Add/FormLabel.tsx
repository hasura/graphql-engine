import React from 'react';
import { ToolTip } from '@/new-components/Tooltip';

type FormLabelProps = {
  title: string;
  tooltip: string;
};

const FormLabel: React.FC<FormLabelProps> = ({ title, tooltip }) => {
  return (
    <>
      <h2 className="text-lg font-semibold mb-xs flex items-center">
        {title}
        <ToolTip message={tooltip} />
      </h2>
    </>
  );
};

export default FormLabel;
