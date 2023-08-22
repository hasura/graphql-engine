import { TbMathFunction } from 'react-icons/tb';
import { QualifiedFunction } from '../../../../hasura-metadata-types';
import { functionDisplayName } from '../utils';

export const FunctionDisplayName = ({
  dataSourceName,
  qualifiedFunction,
}: {
  dataSourceName?: string;
  qualifiedFunction: QualifiedFunction;
}) => {
  return (
    <div className="flex gap-1 items-center">
      <TbMathFunction className="text-muted mr-xs" />
      {functionDisplayName({ dataSourceName, qualifiedFunction })}
    </div>
  );
};
