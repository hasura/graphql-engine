import { Dialog, Flex, Strong } from '@radix-ui/themes';
import React, { ReactNode } from 'react';
import { Button, ButtonProps } from '../Button';
import clsx from 'clsx';

export type FooterProps = {
  callToAction?: React.ReactNode;
  callToActionProps?: Omit<ButtonProps, 'children' | 'onClick'>;
  callToDeny?: React.ReactNode;
  callToDenyProps?: Omit<ButtonProps, 'children' | 'onClick'>;
  onSubmit?: React.MouseEventHandler<HTMLButtonElement>;
  onClose?: () => void;
  isLoading?: boolean;
  className?: string;
  onSubmitAnalyticsName?: string;
  onCancelAnalyticsName?: string;
  disabled?: boolean;
  leftContent?: ReactNode;
};

const DialogFooter: React.FC<FooterProps> = ({
  callToAction,
  callToActionProps,
  callToDeny,
  callToDenyProps,
  onSubmit,
  onClose,
  isLoading = false,
  className,
  disabled = false,
  leftContent,
}) => {
  return (
    <Flex
      align="center"
      justify={leftContent ? 'between' : 'end'}
      className={clsx('mt-4', className)}
    >
      {leftContent && <div>{leftContent}</div>}
      <Flex align="center" gap="4">
        {callToDeny && (
          <Dialog.Close onClick={onClose}>
            <Button
              color="gray"
              variant="ghost"
              disabled={disabled || isLoading}
              {...callToDenyProps}
            >
              <Strong>{callToDeny}</Strong>
            </Button>
          </Dialog.Close>
        )}
        <Button
          disabled={disabled}
          type="submit"
          mode="primary"
          loading={isLoading}
          {...callToActionProps}
          onClick={onSubmit}
        >
          {callToAction}
        </Button>
      </Flex>
    </Flex>
  );
};

export default DialogFooter;
