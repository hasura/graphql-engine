import {
  IconTooltip,
  Separator,
  Text,
  Checkbox,
  GraphqlCodeBlock,
} from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

const tooltip =
  'This code is generated based on the assumption that operation was derived from another operation. If the assumption is wrong, you can disable the derivation.';

const DerivedFrom = ({
  shouldDerive,
  parentMutation,
  toggleDerivation,
}: {
  shouldDerive: boolean;
  parentMutation: string;
  toggleDerivation: () => void;
}) => {
  if (!parentMutation) return null;
  return (
    <Flex direction="column" gap="4">
      <Separator size="4" />
      <Flex align="center" gap="2">
        <Text weight="bold">Derived operation</Text>
        <IconTooltip message={tooltip} />
      </Flex>
      <div>
        <Checkbox
          id="derivedFromInputId"
          value={shouldDerive}
          onChange={toggleDerivation}
        >
          Generate code with delegation to the derived mutation
        </Checkbox>
      </div>
      <GraphqlCodeBlock text={parentMutation} />
    </Flex>
  );
};

export default DerivedFrom;
