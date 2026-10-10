import { IconTooltip } from '../Tooltip';
import { Flex, Dialog as ThemeDialog } from '@radix-ui/themes';
import clsx from 'clsx';
import React, { ReactElement, useEffect, useState } from 'react';
import { FaTimes } from 'react-icons/fa';
import DialogFooter, { FooterProps } from './DialogFooter';
import { Text } from '../typography';
import { IconButton } from '../Button';
import { Separator } from '../Separator';

type DialogSize = 'sm' | 'md' | 'lg' | 'xl' | 'xxl' | 'xxxl' | 'max';

const dialogSizing: Record<DialogSize, string> = {
  sm: '36rem',
  md: '42rem',
  lg: '48rem',
  xl: '56rem',
  xxl: '64rem',
  xxxl: '72rem',
  max: '100%',
};

export type DialogProps = Omit<ThemeDialog.RootProps, 'children'> & {
  className?: string;
  children: React.ReactNode | (() => React.ReactNode);
  title?: React.ReactNode;
  titleTooltip?: string;
  description?: React.ReactNode;
  onClose?: () => void;
  footer?: FooterProps | ReactElement<any>;
  size?: DialogSize;
  separator?: boolean;
  // provides a way to add styles to the div wrapping the
  contentContainer?: {
    className?: string;
  };
};

export const Dialog = React.forwardRef<HTMLDivElement, DialogProps>(
  (
    {
      className,
      children,
      title,
      titleTooltip,
      description,
      onClose,
      footer,
      size = 'md',
      contentContainer,
      open = true,
      separator,
      ...rootProps
    },
    ref,
  ) => {
    return (
      <ThemeDialog.Root open={open} {...rootProps}>
        <ThemeDialog.Content
          ref={ref}
          maxWidth={dialogSizing[size]}
          className={clsx('overflow-hidden', className)}
        >
          {onClose && (
            <ThemeDialog.Close onClick={onClose}>
              <div className="absolute right-6 top-8">
                <IconButton color="gray" variant="ghost" radius="full">
                  <FaTimes className="w-6 h-6" />
                </IconButton>
              </div>
            </ThemeDialog.Close>
          )}
          {!!title && (
            <ThemeDialog.Title className="py-2">
              <Flex align="center" gap="2">
                <Text size="4" weight="bold">
                  {title}
                </Text>
                {!!titleTooltip && <IconTooltip message={titleTooltip} />}
              </Flex>
              {Boolean(separator) && <Separator size="4" className="mt-4" />}
            </ThemeDialog.Title>
          )}
          {!!description && (
            <ThemeDialog.Description className="py-4">
              {description}
            </ThemeDialog.Description>
          )}
          <div
            className={clsx(
              'overflow-y-auto max-h-full',
              contentContainer?.className,
            )}
          >
            {typeof children === 'function' ? children() : children}
          </div>
          {!footer ? null : React.isValidElement(footer) ? (
            footer
          ) : (
            <DialogFooter {...footer} />
          )}
        </ThemeDialog.Content>
      </ThemeDialog.Root>
    );
  },
);

export const DelayedDialog = ({
  delay = 100,
  children,
  ...props
}: DialogProps & {
  delay?: number;
}) => {
  const [visible, setVisible] = useState(false);

  useEffect(() => {
    setTimeout(() => {
      setVisible(true);
    }, delay);
  }, []);

  return <Dialog {...props}>{visible ? children : null}</Dialog>;
};

export { DialogFooter };
