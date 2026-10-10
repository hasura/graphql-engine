import {
  Text as ThemeText,
  TextProps as ThemeTextProps,
} from '@radix-ui/themes';

export type TextProps = ThemeTextProps;

export const Text = ({ size = '2', ...rest }: TextProps) => {
  return <ThemeText size={size} {...rest} />;
};
