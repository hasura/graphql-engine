import { IconType } from 'react-icons';
import { Button } from '@hasura/shared/ui';
import { Link } from '@radix-ui/themes';

export type LinkButtonProps = {
  url: string;
  buttonText: string;
  icon?: IconType;
  iconPosition?: 'start' | 'end';
  id?: string;
};

export function LinkButton(props: LinkButtonProps) {
  const { id, url, buttonText, icon, iconPosition } = props;
  return (
    <Link href={url} target="_blank" rel="noopener noreferrer">
      <Button
        id={id}
        mode="default"
        {...(iconPosition === 'end'
          ? {
              rightIcon: icon,
            }
          : {
              leftIcon: icon,
            })}
      >
        {buttonText}
      </Button>
    </Link>
  );
}
