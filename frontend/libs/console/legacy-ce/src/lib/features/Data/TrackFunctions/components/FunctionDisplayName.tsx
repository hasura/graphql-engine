import { TbMathFunction } from 'react-icons/tb';
import { QualifiedFunction } from '../../../hasura-metadata-types';
import { adaptFunctionName } from '../utils';

export const FunctionDisplayName = ({
  dataSourceName,
  qualifiedFunction,
}: {
  dataSourceName?: string;
  qualifiedFunction: QualifiedFunction;
}) => {
  const functionName = adaptFunctionName(qualifiedFunction);

  if (!dataSourceName)
    return (
      <div className="flex gap-1 items-center">
        <TbMathFunction className="text-2xl text-gray-600" />
        {functionName.join(' / ')}
      </div>
    );

  return (
    <div className="flex gap-1 items-center">
      <TbMathFunction className="text-2xl text-gray-600" />
      {dataSourceName} / {functionName.join(' / ')}
    </div>
  );
};
