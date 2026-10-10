import { RiInformationFill } from 'react-icons/ri';
import { Flex } from '@radix-ui/themes';
import { Collapsible, CollapsibleProps, IconTooltip } from '@hasura/shared/ui';

export const CollapsibleResource: React.FC<
  {
    title: React.ReactNode;
    tooltip: string;
  } & Omit<CollapsibleProps, 'triggerChildren'>
> = ({ title, tooltip, children, ...rest }) => (
  <Collapsible
    triggerChildren={
      <Flex align="center" className="mb-1">
        <div className="font-semibold inline-flex items-center text-lg">
          {title}
        </div>
        <IconTooltip
          icon={<RiInformationFill />}
          message={tooltip}
          side="right"
        />
      </Flex>
    }
    {...rest}
  >
    {children}
  </Collapsible>
);
