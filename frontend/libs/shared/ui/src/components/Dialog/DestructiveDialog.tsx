import { Strong } from '@radix-ui/themes';
import { ButtonProps } from '../Button';
import { Dialog, DialogFooter, DialogProps } from './Dialog';
import { Text } from '../typography';
import { ReactNode, useState } from 'react';
import { Checkbox } from '../Form';
import { IconTooltip } from '../Tooltip';

export type DestructiveDialogArgs = Omit<DialogProps, 'open' | 'footer'> & {
  confirmButtonProps?: Omit<ButtonProps, 'onClick'>;
  cancelButtonProps?: Omit<ButtonProps, 'onClick'>;
  onConfirm: () => void;
  onClose: () => void;
  loading?: boolean;
  disabled?: boolean;
};

export const getDestructiveDescription = ({
  destroyTerm = 'remove',
  resourceType,
  resourceName,
}: {
  destroyTerm: string;
  resourceType: string;
  resourceName: string;
}) => (
  <Text as="div">
    Are you sure you want to {destroyTerm} {resourceType}:{' '}
    <Strong>{resourceName}</Strong>?
  </Text>
);

const defaultDestructiveMessage = (
  <Text as="div">Are you sure you want to remove this resource?</Text>
);

export const DestructiveDialog = ({
  disabled,
  loading,
  children,
  confirmButtonProps,
  cancelButtonProps,
  onClose,
  onConfirm,
  title = 'Remove resource',
  ...rest
}: DestructiveDialogArgs) => (
  <Dialog
    {...rest}
    open
    title={title}
    footer={
      <DialogFooter
        callToAction={confirmButtonProps?.children ?? 'Remove'}
        callToActionProps={{
          mode: 'destructive',
          loadingText: 'Removing...',
          ...confirmButtonProps,
        }}
        callToDeny={cancelButtonProps?.children ?? <Strong>Cancel</Strong>}
        callToDenyProps={{
          variant: 'ghost',
          ...confirmButtonProps,
        }}
        disabled={disabled}
        isLoading={loading}
        onClose={onClose}
        onSubmit={onConfirm}
      />
    }
  >
    {children ?? defaultDestructiveMessage}
  </Dialog>
);

export const DestructiveDialogCascade = ({
  onConfirm,
  children,
  ...props
}: Omit<DestructiveDialogArgs, 'onConfirm'> & {
  onConfirm: (cascade: boolean) => void;
}) => {
  const [cascade, setCascade] = useState(false);

  return (
    <DestructiveDialog {...props} onConfirm={() => onConfirm(cascade)}>
      {(children as ReactNode) ?? defaultDestructiveMessage}
      <div className="mt-4">
        <Checkbox
          value={cascade}
          onChange={(value) => setCascade!(value === true)}
        >
          Enable cascade?{' '}
          <IconTooltip message="When set to true, the effect (if possible) is cascaded to any Metadata dependent objects (relationships, permissions, templates)." />
        </Checkbox>
      </div>
    </DestructiveDialog>
  );
};
