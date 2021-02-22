import {
  ArgType,
  PGFunction,
  PGInputArgType,
} from '../../../../dataSources/services/postgresql/types';

export const getTrackableFunctions = (
  functionsList: PGFunction[],
  trackedFunctions: PGFunction[]
): PGFunction[] => {
  const trackedFuncNames = trackedFunctions.map(fn => fn.function_name);
  const containsTableArgs = (arg: PGInputArgType): boolean =>
    arg.type.toLowerCase() === ArgType.CompositeType;
  // Assuming schema for both function and tables are same
  // return function which are tracked
  const filterCondition = (func: PGFunction) => {
    return (
      !trackedFuncNames.includes(func.function_name) &&
      !func.input_arg_types?.some(containsTableArgs)
    );
  };
  return functionsList.filter(filterCondition);
};
