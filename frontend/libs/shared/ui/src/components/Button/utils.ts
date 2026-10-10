import { ButtonProps } from '@radix-ui/themes';

export type ButtonModes = 'default' | 'destructive' | 'primary' | 'success';

export const buttonModeColors: Record<ButtonModes, ButtonProps> = {
  default: {
    color: 'gray',
    variant: 'surface',
    highContrast: true,
  },
  destructive: {
    color: 'red',
  },
  success: {
    color: 'green',
  },
  primary: {
    color: 'indigo',
  },
};
