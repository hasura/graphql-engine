import React, { ReactElement } from 'react';
import { Button } from '../../../../../../new-components/Button';

export type LinkButtonProps = {
  url: string;
  buttonText: string;
  icon?: ReactElement;
  iconPosition?: 'start' | 'end';
  id?: string;
};

export function LinkButton(props: LinkButtonProps) {
  const { id, url, buttonText, icon, iconPosition } = props;
  return (
    <a href={url} target="_blank" rel="noopener noreferrer">
      <Button
        id={id}
        mode="default"
        className="mr-sm bg-none !bg-transparent border-red-500"
        icon={icon}
        iconPosition={iconPosition}
      >
        <span className="text-white font-semibold text-md">{buttonText}</span>
      </Button>
    </a>
  );
}
