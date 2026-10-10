import React from 'react';
import clsx from 'clsx';
import {
  Card as ThemeCard,
  CardProps as ThemeCardProps,
} from '@radix-ui/themes';

type CardMode = 'default' | 'neutral' | 'positive' | 'error' | 'warning';

interface CardProps extends ThemeCardProps {
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

const modeAccentClassNames: Record<CardMode, string> = {
  default: '',
  neutral: 'bg-secondary',
  positive: 'bg-emerald-600',
  error: 'bg-red-600',
  warning: 'bg-amber-500',
};

export const Card: React.FC<CardProps> = ({
  mode = 'default',
  disabled = false,
  children,
  ...otherHtmlAttributes
}) => {
  return (
    <ThemeCard
      {...otherHtmlAttributes}
      className={clsx(
        'relative',
        disabled && 'bg-gray-200 dark:bg-slate-700',
        otherHtmlAttributes.className,
        {
          'cursor-pointer hover:shadow-md':
            otherHtmlAttributes.onClick && !disabled,
        },
      )}
    >
      {mode !== 'default' && (
        <span
          className={clsx(
            'absolute inset-y-0 left-0 w-0.5',
            modeAccentClassNames[mode],
          )}
        />
      )}
      {children}
    </ThemeCard>
  );
};
