import { IconType } from 'react-icons';

export type ReactSelectOptionType<V = any> = {
  icon?: IconType;
  label: string;
  value: V;
  isDisabled?: boolean;
};
