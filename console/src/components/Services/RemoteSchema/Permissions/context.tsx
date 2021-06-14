import React from 'react';
import { ArgTreeType } from './types';

interface PermissionEditorContextType {
  argTree?: ArgTreeType;
  setArgTree?: React.Dispatch<React.SetStateAction<ArgTreeType>>;
  scrollToElement?: (s: string) => void;
}
export const PermissionEditorContext = React.createContext<PermissionEditorContextType>(
  {}
);
