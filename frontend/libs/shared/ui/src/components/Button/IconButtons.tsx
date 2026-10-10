import { FaTimesCircle } from 'react-icons/fa';
import { IconButton, IconButtonProps } from './IconButton';

export const IconButtonDelete = ({
  color = 'indigo',
  variant = 'ghost',
  radius = 'full',
  ...rest
}: Omit<IconButtonProps, 'children'>) => {
  return (
    <IconButton color={color} variant={variant} radius={radius} {...rest}>
      <FaTimesCircle />
    </IconButton>
  );
};
