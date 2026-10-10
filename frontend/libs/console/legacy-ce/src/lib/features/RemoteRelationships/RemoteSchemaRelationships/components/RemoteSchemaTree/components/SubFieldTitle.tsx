import { IconTooltip, Text } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

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
    <Flex align="center" className="cursor-pointer w-max whitespace-nowrap">
      {!enabled ? (
        <>
          <IconTooltip
            className="mr-2 text-gray-400"
            message="Only fields with arguments or subfields can be toggled"
          />
          <span className="text-gray-400">{title}</span>
        </>
      ) : isSubfield ? (
        <Text color="blue" className="hover:text-blue-600!">
          {title}
        </Text>
      ) : (
        <span>{title}</span>
      )}
    </Flex>
  );
};
