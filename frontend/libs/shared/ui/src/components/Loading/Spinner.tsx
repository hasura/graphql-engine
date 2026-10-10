import {
  Spinner as ThemeSpinner,
  SpinnerProps as ThemeSpinnerProps,
} from '@radix-ui/themes';

export type SpinnerProps = ThemeSpinnerProps;

export const Spinner = ({ size = '2', ...rest }: SpinnerProps) => {
  return <ThemeSpinner {...rest} />;
};
