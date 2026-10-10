import {
  IconButton as ThemeIconButton,
  IconButtonProps as ThemeIconButtonProps,
} from '@radix-ui/themes';
import { forwardRef, Ref } from 'react';
import { buttonModeColors, ButtonModes } from './utils';
import { IconType } from 'react-icons';

const getIconSize = (size: ThemeIconButtonProps['size']) => {
  switch (size) {
    case '1':
      return 'w-3 h-3';
    case '3':
      return 'w-5 h-5';
    case '4':
      return 'w-6 h-6';
    case '2':
    default:
      return 'w-4 h-4';
  }
};

export interface IconButtonProps extends ThemeIconButtonProps {
  /**
   * The button mode
   */
  mode?: ButtonModes;
  /**
   * The button icon
   */
  icon?: IconType | null;
}

export const IconButton = forwardRef<HTMLButtonElement, IconButtonProps>(
  (
    {
      mode,
      type = 'button',
      icon: Icon,
      children,
      size = '2',
      ...props
    }: IconButtonProps,
    ref,
  ) => {
    const modeProps = mode ? buttonModeColors[mode] : undefined;
    return (
      <ThemeIconButton
        {...modeProps}
        {...props}
        type={type}
        size={size}
        ref={ref as Ref<HTMLButtonElement>}
      >
        {Icon ? <Icon className={getIconSize(size)} /> : children}
      </ThemeIconButton>
    );
  },
);
