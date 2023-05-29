import { TbMathFunction } from 'react-icons/tb';
import { QualifiedFunction } from '../../../../hasura-metadata-types';
import { adaptFunctionName } from '../utils';

export const FunctionDisplayName = ({
  dataSourceName,
  qualifiedFunction,
}: {
  dataSourceName?: string;
  qualifiedFunction: QualifiedFunction;
}) => {
  const functionName = adaptFunctionName(qualifiedFunction);

  const name = dataSourceName
    ? `${dataSourceName} / ${functionName.join(' / ')}`
    : functionName.join(' / ');

  return (
    <div className="flex gap-1 items-center">
      <TbMathFunction className="text-muted mr-xs" />
      {name}
    </div>
  );
};
