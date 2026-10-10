import { ValueInputType } from './ValueInputType';
import { InputSuggestion } from './InputSuggestion';
import { SelectTable } from './SelectTable';
import { Flex } from '@radix-ui/themes';

export const ValueInput = ({ value, path }: { value: any; path: string[] }) => {
  const comparatorName = path[path.length - 1];
  const componentLevelId = `${path.join('.')}-value-input`;

  if (comparatorName === '_table') {
    return (
      <SelectTable
        componentLevelId={componentLevelId}
        path={path}
        value={value}
      />
    );
  }

  return (
    <Flex align="center">
      <ValueInputType
        componentLevelId={componentLevelId}
        path={path}
        comparatorName={comparatorName}
        value={value}
      />
      <InputSuggestion
        comparatorName={comparatorName}
        path={path}
        componentLevelId={componentLevelId}
      />
    </Flex>
  );
};
