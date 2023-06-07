export type BoxProps = Omit<React.ComponentPropsWithRef<'div'>, 'color'>;

export const Box = 'div' as any as React.FC<BoxProps>;
