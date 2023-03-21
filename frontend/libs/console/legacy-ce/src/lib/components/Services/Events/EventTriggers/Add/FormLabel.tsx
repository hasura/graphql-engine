import React from 'react';
import { IconTooltip } from '../../../../../new-components/Tooltip';
import { LearnMoreLink } from '../../../../../new-components/LearnMoreLink';

type FormLabelProps = {
  title: string;
  tooltip: string;
  tooltipIcon?: React.ReactElement;
  knowMoreLink?: string;
};

const FormLabel: React.FC<FormLabelProps> = ({
  title,
  tooltip,
  tooltipIcon,
  knowMoreLink,
}) => {
  return (
    <h2 className="text-lg font-semibold mb-xs flex items-center">
      {title}
      <IconTooltip message={tooltip} icon={tooltipIcon} />
      {knowMoreLink ? <LearnMoreLink href={knowMoreLink} /> : null}
    </h2>
  );
};

export default FormLabel;
