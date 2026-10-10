import {
  Separator as ThemeSeparator,
  SeparatorProps as ThemeSeparatorProps,
} from '@radix-ui/themes';

export type SeparatorProps = ThemeSeparatorProps;

export const Separator = (props: SeparatorProps) => {
  return (
    <ThemeSeparator
      style={{
        opacity: '0.5',
      }}
      {...props}
    />
  );
};
