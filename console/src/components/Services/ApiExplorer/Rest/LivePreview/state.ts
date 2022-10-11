import { VariableData } from '../utils';
import { DataHeader } from './.';

export interface HeaderState extends DataHeader {
  index: number;
}

export interface VariableState extends VariableData {
  value: string;
}

export type LoadingState = {
  isLoading: boolean;
  success: boolean;
  data?: string;
  error?: {
    message: string;
    status: number;
  };
};

export const defaultHeaderState: HeaderState[] = [];
export const defaultVariableState: VariableState[] = [];
export const defaultLoadingState: LoadingState = {
  isLoading: false,
  success: false,
};
