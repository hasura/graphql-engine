import { createContext } from 'react';
import { LogicalModel } from '../../../../../hasura-metadata-types';

type LogicalModelState = {
  rootLogicalModel: LogicalModel | undefined;
  logicalModels: LogicalModel[];
  logicalModel: LogicalModel['name'] | undefined;
};

export const logicalModelContext = createContext<LogicalModelState>({
  logicalModel: '',
  logicalModels: [],
  rootLogicalModel: undefined,
});

export const RootLogicalModelProvider = ({
  children,
  logicalModel,
  logicalModels,
}: Omit<LogicalModelState, 'rootLogicalModel'> & {
  children?: React.ReactNode | undefined;
}) => {
  const rootLogicalModel = logicalModels.find(t => t.name === logicalModel);
  return (
    <logicalModelContext.Provider
      value={{
        logicalModel,
        logicalModels,
        rootLogicalModel,
      }}
    >
      {children}
    </logicalModelContext.Provider>
  );
};
