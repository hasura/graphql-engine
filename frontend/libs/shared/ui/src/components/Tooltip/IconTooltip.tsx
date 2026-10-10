import React, { ReactNode } from 'react';
import { FaQuestionCircle } from 'react-icons/fa';
import { IconButton, IconButtonProps } from '../Button';
import { Tooltip, TooltipProps } from '@radix-ui/themes';

export type IconTooltipProps = {
  /**
   * The tooltip message
   */
  message: React.ReactNode;
  /**
   * tooltip icon other then ?
   */
  icon?: ReactNode;

  iconProps?: Omit<IconButtonProps, 'type' | 'onClick'>;
} & Omit<TooltipProps, 'content'>;

export const IconTooltip: React.FC<IconTooltipProps> = ({
  className,
  message,
  icon,
  side = 'right',
  align = 'center',
  iconProps,
  ...rest
}) => {
  const {
    variant = 'ghost',
    color = 'gray',
    className: iconClassName = 'cursor-pointer',
    radius = 'full',
    ...iconRest
  } = iconProps ?? {};

  return (
    <Tooltip {...rest} content={message}>
      {
        <IconButton
          type="button"
          variant={variant}
          color={color}
          radius={radius}
          className={iconClassName}
          {...iconRest}
        >
          {icon || <FaQuestionCircle className={className} />}
        </IconButton>
      }
    </Tooltip>
  );
};
