import { Flex, FlexProps } from '@radix-ui/themes';
import { IconTooltip } from '../Tooltip';
import { Text } from '../typography';

type Props = FlexProps & {
  /**
   * The collapse header title
   */
  title?: string;
  /**
   * The collapse header tooltip content
   */
  tooltip?: string;
  /**
   * The collapse header status
   */
  status?: string;
};

export const CollapsibleHeader = ({
  title,
  tooltip,
  status,
  align = 'center',
  gap = '2',
  ...rest
}: Props) => (
  <Flex gap={gap} align={align} {...rest}>
    <Text weight="bold">{title}</Text>
    {tooltip && status ? (
      <>
        {!!tooltip && <IconTooltip message={tooltip} />}
        {!!status && <Text size="1">{status}</Text>}
      </>
    ) : null}
  </Flex>
);
