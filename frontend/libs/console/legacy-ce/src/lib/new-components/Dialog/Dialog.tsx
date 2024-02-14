import { useGetAnalyticsAttributes } from '../../features/Analytics';
import { IconTooltip } from '../Tooltip';
import * as RadixDialog from '@radix-ui/react-dialog';
import clsx from 'clsx';
import React, { ReactElement } from 'react';
import { FaTimes } from 'react-icons/fa';
import { Button, ButtonProps } from '../Button/Button';

export type FooterProps = {
  callToAction?: string;
  callToActionLoadingText?: string;
  callToActionIcon?: ButtonProps['icon'];
  callToActionIconPosition?: ButtonProps['iconPosition'];
  callToDeny?: string;
  callToActionType?: ButtonProps['type'];
  onSubmit?: () => void;
  onClose?: () => void;
  isLoading?: boolean;
  className?: string;
  onSubmitAnalyticsName?: string;
  onCancelAnalyticsName?: string;
  disabled?: boolean;
  leftContent?: string | ReactElement;
  id?: string;
};

const noop = () => null;

const Footer: React.VFC<FooterProps> = ({
  callToAction,
  callToActionLoadingText = '',
  callToActionIcon,
  callToActionIconPosition,
  callToDeny,
  callToActionType = 'submit',
  onClose,
  onSubmit,
  isLoading = false,
  className,
  onSubmitAnalyticsName,
  onCancelAnalyticsName,
  disabled = false,
  leftContent,
  id,
}) => {
  const callToActionProps: ButtonProps = {
    icon: callToActionIcon,
    iconPosition: callToActionIconPosition,
    onClick: onSubmit || noop,
  };

  const onSubmitAnalyticsAttributes = useGetAnalyticsAttributes(
    onSubmitAnalyticsName
  );

  const onCancelAnalyticsAttributes = useGetAnalyticsAttributes(
    onCancelAnalyticsName
  );

  return (
    <div
      id={id}
      className={clsx(
        'flex items-center justify-end border-t border-gray-300 bg-white p-sm',
        className
      )}
    >
      {leftContent && <div className="justify-self-start">{leftContent}</div>}
      <div className="grow"></div>
      {callToDeny && (
        <div className="mr-1.5">
          <Button onClick={onClose} {...onCancelAnalyticsAttributes}>
            {callToDeny}
          </Button>
        </div>
      )}
      <Button
        disabled={disabled}
        {...callToActionProps}
        type={callToActionType}
        mode="primary"
        isLoading={isLoading}
        loadingText={callToActionLoadingText}
        {...onSubmitAnalyticsAttributes}
      >
        {callToAction}
      </Button>
    </div>
  );
};

const Backdrop = () => (
  <RadixDialog.Overlay className="fixed top-0 left-0 h-full w-full bg-gray-900/90 backdrop-blur-sm z-[100]" />
);

type DialogSize = 'sm' | 'md' | 'lg' | 'xl' | 'xxl' | 'xxxl' | 'max';

const dialogSizing: Record<DialogSize, string> = {
  sm: 'max-w-xl',
  md: 'max-w-2xl',
  lg: 'max-w-3xl',
  xl: 'max-w-4xl',
  xxl: 'max-w-5xl',
  xxxl: 'max-w-6xl',
  max: 'max-w-max',
};

export type DialogProps = {
  children: string | ReactElement;
  title?: string | ReactElement;
  titleTooltip?: string;
  description?: string | ReactElement;
  hasBackdrop?: boolean;
  onClose?: () => void;
  footer?: FooterProps | ReactElement;
  size?: DialogSize;
  portal?: boolean;
  // provides a way to add styles to the div wrapping the
  contentContainer?: {
    className?: string;
  };
};

const DialogChildren = (props: DialogProps) => {
  const {
    children,
    hasBackdrop,
    title,
    titleTooltip,
    description,
    onClose,
    footer,
    size = 'md',
    contentContainer,
  } = props;

  return (
    <>
      {hasBackdrop && <Backdrop />}
      <RadixDialog.Content
        className={clsx(
          size === 'max' ? '' : 'w-full',
          `fixed transform -translate-x-2/4 left-2/4 mt-lg top-0 h-auto  bg-gray-50 rounded overflow-hidden shadow-lg z-[101]`,
          dialogSizing[size]
        )}
      >
        {onClose && (
          <RadixDialog.Close className="fixed right-3 top-3">
            <FaTimes
              className="ml-auto w-5 h-5 cursor-pointer text-gray-400 fill-current hover:text-gray-500"
              onClick={onClose}
            />
          </RadixDialog.Close>
        )}
        {!!title && (
          <RadixDialog.Title className="flex items-top mb-1 pl-md pt-md pr-md text-xl font-semibold">
            {title}
            {!!titleTooltip && (
              <IconTooltip className="-ml-1" message={titleTooltip} />
            )}
          </RadixDialog.Title>
        )}
        {!!description && (
          <RadixDialog.Description className="text-muted pl-md pb-md pr-md ">
            {description}
          </RadixDialog.Description>
        )}
        <div
          className={clsx(
            'overflow-y-auto max-h-[calc(100vh-14rem)]',
            contentContainer?.className
          )}
        >
          {children}
        </div>
        {React.isValidElement(footer) && footer}
        {footer && !React.isValidElement(footer) && <Footer {...footer} />}
      </RadixDialog.Content>
    </>
  );
};

export const Dialog = (props: DialogProps) => {
  const { portal = false } = props;
  return (
    <RadixDialog.Root open>
      {portal ? (
        <RadixDialog.Portal>
          <DialogChildren {...props} />
        </RadixDialog.Portal>
      ) : (
        <DialogChildren {...props} />
      )}
    </RadixDialog.Root>
  );
};

Dialog.Footer = Footer;
