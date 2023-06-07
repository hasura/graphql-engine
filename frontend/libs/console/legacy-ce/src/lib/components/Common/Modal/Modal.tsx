import React from 'react';
import * as RadixDialog from '@radix-ui/react-dialog';
import { Button } from './../../../new-components/Button';
import { FaTimes } from 'react-icons/fa';
import clsx from 'clsx';

export interface ModalProps {
  show?: boolean;
  title: React.ReactElement;
  children: React.ReactElement;
  onClose?(): void;
  onSubmit?(): void;
  onCancel?(): void;
  customClass?: string;
  submitText?: React.ReactElement | string;
  leftActions?: React.ReactElement;
  submitTestId?: string;
}
const Modal = ({
  show = true,
  title,
  onClose,
  customClass = '',
  onSubmit,
  onCancel,
  submitText = '',
  leftActions,
  submitTestId = '',
  children,
}: ModalProps) => {
  const getHeader = () => {
    return (
      <RadixDialog.Title className="p-4 border-b border-gray-300 text-lg">
        {title}
      </RadixDialog.Title>
    );
  };

  const getBody = () => {
    return (
      <RadixDialog.Description className=" p-4">
        {children}
      </RadixDialog.Description>
    );
  };

  const triggerOnClose = () => {
    if (onCancel) {
      onCancel();
    } else if (onClose) {
      onClose();
    }
  };

  const getFooter = () => {
    if (!onSubmit) {
      return null;
    }

    return (
      <div>
        <div className="flex items-center justify-between p-4 border-t border-gray-300">
          <div>{leftActions ?? null}</div>
          <div className="flex gap-2">
            <Button onClick={triggerOnClose}>Cancel</Button>
            <Button onClick={onSubmit} mode="primary" data-test={submitTestId}>
              {submitText || 'Submit'}
            </Button>
          </div>
        </div>
      </div>
    );
  };

  const bootstrapModalProps: RadixDialog.DialogProps = {
    open: show,
  };

  return (
    <RadixDialog.Root {...bootstrapModalProps}>
      <RadixDialog.Portal>
        <RadixDialog.Overlay className="fixed top-0 left-0 h-full w-full bg-gray-900/90 backdrop-blur-sm z-[100]" />
        <RadixDialog.Content
          className={clsx(
            'fixed transform w-1/2 -translate-x-2/4 left-2/4 mt-lg top-0 h-auto bg-gray-50 rounded shadow-lg z-[101]',
            customClass
          )}
        >
          <RadixDialog.Close className="fixed bg-legacybg rounded-full right-3 top-3">
            <FaTimes
              className="ml-auto w-5 h-5 cursor-pointer text-gray-400 fill-current hover:text-gray-500"
              onClick={onClose}
            />
          </RadixDialog.Close>
          {getHeader()}
          {getBody()}
          {getFooter()}
        </RadixDialog.Content>
      </RadixDialog.Portal>
    </RadixDialog.Root>
  );
};

Modal.Button = Button;

export default Modal;
