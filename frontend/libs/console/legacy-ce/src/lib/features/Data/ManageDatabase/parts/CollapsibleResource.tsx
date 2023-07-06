import { RiInformationFill } from 'react-icons/ri';
import {
  Collapsible,
  CollapsibleProps,
} from '../../../../new-components/Collapsible';
import { IconTooltip } from '../../../../new-components/Tooltip';

export const CollapsibleResource: React.FC<
  {
    title: React.ReactNode;
    tooltip: string;
  } & Omit<CollapsibleProps, 'triggerChildren'>
> = ({ title, tooltip, children, ...rest }) => (
  <Collapsible
    triggerChildren={
      <div className="flex mb-1 items-center">
        <div className="font-semibold inline-flex items-center text-lg">
          {title}
        </div>
        <IconTooltip
          icon={<RiInformationFill />}
          message={tooltip}
          side="right"
        />
      </div>
    }
    {...rest}
  >
    {children}
  </Collapsible>
);
