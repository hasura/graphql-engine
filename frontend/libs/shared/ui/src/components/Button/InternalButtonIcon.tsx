import { ButtonProps } from '@radix-ui/themes';
import clsx from 'clsx';
import { IconType } from 'react-icons';

type IconSize = 'sm' | 'md' | 'lg' | ButtonProps['size'];

const getIconSize = (size: IconSize) => {
  switch (size) {
    case '1':
    case 'sm':
      return 'w-3 h-3';
    case '3':
    case 'lg':
      return 'w-5 h-5';
    case '4':
      return 'w-6 h-6';
    case '2':
    case 'md':
    default:
      return 'w-4 h-4';
  }
};

function ButtonIcon(props: {
  className?: string;
  icon: IconType;
  size?: IconSize;
  buttonHasChildren: boolean;
  iconPosition?: 'start' | 'end';
}) {
  const { icon: Icon, iconPosition, buttonHasChildren, size } = props;

  const className = clsx('inline-flex', props.className, getIconSize(size), {
    'mr-1': buttonHasChildren && iconPosition === 'start',
    'ml-1': buttonHasChildren && iconPosition === 'end',
  });

  return <Icon className={className} />;
}

export default ButtonIcon;
