import { IconTooltip } from '../../../../../../../../../new-components/Tooltip';
import React from 'react';

type SubFieldTitleProps = {
  title: string;
  enabled?: boolean;
  isSubfield?: boolean;
};

export const SubFieldTitle = ({
  title,
  enabled,
  isSubfield,
}: SubFieldTitleProps) => {
  return (
    <div className="flex items-center cursor-pointer w-max whitespace-nowrap">
      {!enabled ? (
        <>
          <IconTooltip
            className="mr-sm text-gray-400"
            message="Only fields with arguments or subfields can be toggled"
          />
          <span className="text-gray-400">{title}</span>
        </>
      ) : isSubfield ? (
        <span className="text-blue-600 hover:text-blue-900">{title}</span>
      ) : (
        <span className="text-gray-900">{title}</span>
      )}
    </div>
  );
};
