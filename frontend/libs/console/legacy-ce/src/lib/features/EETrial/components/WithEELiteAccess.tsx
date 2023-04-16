import * as React from 'react';
import { useEELiteAccess } from '../hooks/useEELiteAccess';
import { EELiteAccess } from '../types';
import Globals from '../../../Globals';

type Props = {
  children: (result: EELiteAccess) => React.ReactNode;
  globals: typeof Globals;
};
/*
  This component uses the render-prop pattern to allow using
  the logic from `useEELiteAcces` hook in React class copmonents
*/
export const WithEELiteAccess = (props: Props) => {
  const { children, globals } = props;
  const access = useEELiteAccess(globals);
  return children(access);
};
