import React from 'react';
import clsx from 'clsx';

type CardMode = 'default' | 'neutral' | 'positive' | 'error' | 'warning';
interface CardProps extends React.ComponentProps<'div'> {
  /**
   * The card mode
   */
  mode?: CardMode;
  /**
   * Flag to show the card as disabled
   */
  disabled?: boolean;
  /**
   * The component children
   */
  children: React.ReactNode;
}

export const Card: React.FC<React.PropsWithChildren<CardProps>> = ({
  mode = 'default',
  disabled = false,
  children,
  ...otherHtmlAttributes
}) => {
  const decoratedCardClassName =
    'before:w-0.5 before:content-[attr(data-before-content)] before:absolute before:inset-0 before:border-l-2';
  return (
    <div
      {...otherHtmlAttributes}
      data-before-content="&nbsp;"
      className={clsx(
        'rounded p-md relative border border-gray-200',
        disabled ? 'bg-gray-200' : 'bg-white shadow-sm',
        otherHtmlAttributes.className,
        {
          'cursor-pointer hover:shadow-md':
            otherHtmlAttributes.onClick && !disabled,
        },
        {
          default: '',
          neutral: `${decoratedCardClassName} before:border-secondary`,
          positive: `${decoratedCardClassName} before:border-emerald-600`,
          error: `${decoratedCardClassName} before:border-red-600`,
          warning: `${decoratedCardClassName} before:border-amber-500`,
        }[mode]
      )}
    >
      {children}
    </div>
  );
};
