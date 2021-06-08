import { PGFunction } from '../../../../dataSources/services/postgresql/types';

export const getTrackableFunctions = (
  functionsList: PGFunction[],
  trackedFunctions: PGFunction[]
): PGFunction[] => {
  const trackedFuncNames = trackedFunctions.map(fn => fn.function_name);
  const filterCondition = (func: PGFunction) =>
    !trackedFuncNames.includes(func.function_name);
  return functionsList.filter(filterCondition);
};

export const isTrackableAndComputedField = (func: PGFunction) => {
  return (
    func.return_type_type === 'c' &&
    (func.function_type === 'STABLE' || func.function_type === 'IMMUTABLE') &&
    func.returns_set
  );
};
