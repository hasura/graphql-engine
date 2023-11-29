import { ValueInputType } from './ValueInputType';
import { InputSuggestion } from './InputSuggestion';
import { SelectTable } from './SelectTable';

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
    <>
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
    </>
  );
};
