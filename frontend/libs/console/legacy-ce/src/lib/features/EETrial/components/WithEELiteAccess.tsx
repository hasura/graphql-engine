import * as React from 'react';
import { useEELiteAccess } from '../hooks/useEELiteAccess';
import { EELiteAccess } from '../types';

type Props = {
  children: (result: EELiteAccess) => React.ReactNode;
};
/*
  This component uses the render-prop pattern to allow using
  the logic from `useEELiteAcces` hook in React class copmonents
*/
export const WithEELiteAccess = (props: Props) => {
  const { children } = props;
  const access = useEELiteAccess();

  return children(access);
};
