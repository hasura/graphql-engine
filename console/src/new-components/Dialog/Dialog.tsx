import React from 'react';
import * as RadixDialog from '@radix-ui/react-dialog';
import { FaTimes } from 'react-icons/fa';
import { Button } from '../Button/Button';

export type FooterProps = {
  callToAction: string;
  callToActionLoadingText?: string;
  callToDeny?: string;
  onSubmit?: () => void;
  onClose: () => void;
  isLoading: boolean;
};

const Footer: React.FC<FooterProps> = ({
  callToAction,
  callToActionLoadingText = '',
  callToDeny,
  onClose,
  onSubmit,
  isLoading = false,
}) => {
  const callToActionProps = onSubmit ? { onClick: onSubmit } : {};

  return (
    <div className="flex items-center justify-end border-t border-gray-300 bg-white p-sm">
      {callToDeny && (
        <div className="mr-1.5">
          <Button onClick={onClose}>{callToDeny}</Button>
        </div>
      )}
      <Button
        {...callToActionProps}
        type="submit"
        mode="primary"
        isLoading={isLoading}
        loadingText={callToActionLoadingText}
      >
        {callToAction}
      </Button>
    </div>
  );
};

const Backdrop = () => (
  <RadixDialog.Overlay className="fixed top-0 left-0 h-full w-full bg-gray-900/90 backdrop-blur-sm z-[100]" />
);

export type DialogProps = {
  children: string | React.ReactElement;
  title: string;
  description?: string;
  hasBackdrop: boolean;
  onClose?: () => void;
  footer?: FooterProps;
};

export const Dialog = ({
  children,
  hasBackdrop,
  title,
  description,
  onClose,
  footer,
}: DialogProps) => {
  return (
    <RadixDialog.Root open>
      {hasBackdrop && <Backdrop />}
      <RadixDialog.Content className="fixed transform -translate-x-2/4 left-2/4 mt-lg top-0 w-full h-auto max-w-2xl bg-gray-50 rounded overflow-hidden shadow-lg z-[101]">
        {onClose && (
          <RadixDialog.Close className="fixed right-3 top-3">
            <FaTimes
              className="ml-auto w-5 h-5 cursor-pointer text-gray-400 fill-current hover:text-gray-500"
              onClick={onClose}
            />
          </RadixDialog.Close>
        )}
        <RadixDialog.Title className="flex items-top mb-1 pl-sm pt-sm pr-sm text-xl font-semibold">
          {title}
        </RadixDialog.Title>
        <RadixDialog.Description className="text-muted pl-sm pb-sm pr-sm ">
          {description}
        </RadixDialog.Description>
        <div className="overflow-y-auto max-h-[calc(100vh-14rem)]">
          {children}
        </div>
        {footer && <Footer {...footer} />}
      </RadixDialog.Content>
    </RadixDialog.Root>
  );
};

Dialog.Footer = Footer;
