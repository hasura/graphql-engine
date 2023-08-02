import React from 'react';

export type GraphiQLToolbarButtonProps = {
  label: string;
  primary: boolean;
} & React.ComponentProps<'button'>;

export const GraphiQLToolbarButton = React.forwardRef<
  HTMLButtonElement,
  GraphiQLToolbarButtonProps
>((props, forwardedRef) => {
  const [errorMessage, setErrorMessage] = React.useState<string | null>(null);
  const handleClick = (
    event: React.MouseEvent<HTMLButtonElement, MouseEvent>
  ) => {
    try {
      if (props.onClick) props.onClick(event);
      setErrorMessage(null);
    } catch (error: any) {
      setErrorMessage(error.message);
    }
  };

  return (
    <button
      className={
        'toolbar-button' +
        (errorMessage ? ' error' : '') +
        (props.primary ? ' primary' : '')
      }
      title={errorMessage ? errorMessage : props.title}
      onClick={handleClick}
      aria-invalid={errorMessage ? 'true' : 'false'}
    >
      {props.label}
    </button>
  );
});
