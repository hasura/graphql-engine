import { Strong } from '@radix-ui/themes';
import { ButtonProps } from '../Button';
import { Dialog, DialogFooter, DialogProps } from './Dialog';
import { Text } from '../typography';
import { ReactNode, useState } from 'react';
import { Checkbox } from '../Form';
import { IconTooltip } from '../Tooltip';
import { IndicatorCard } from '../IndicatorCard';
import { getDependencyErrors } from '@hasura/shared/utils';

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
  defaultCascade = false,
  error,
  ...props
}: Omit<DestructiveDialogArgs, 'onConfirm'> & {
  onConfirm: (cascade: boolean) => void;
  // Initial state of the cascade checkbox.
  defaultCascade?: boolean;
  // The error of the last attempt. Failures caused by dependent objects are
  // shown in the dialog, asking the user to enable cascade and try again.
  error?: unknown;
}) => {
  const [cascade, setCascade] = useState(defaultCascade);
  const failures = getDependencyErrors(error);
  // only failures caused by dependent objects can be fixed with cascade
  const withDependents = failures?.filter((failure) => failure.objects) ?? [];
  const otherFailures = failures?.filter((failure) => !failure.objects) ?? [];

  return (
    <DestructiveDialog {...props} onConfirm={() => onConfirm(cascade)}>
      {(children as ReactNode) ?? defaultDestructiveMessage}
      {withDependents.length > 0 ? (
        <IndicatorCard
          status="negative"
          showIcon
          className="mt-4"
          headline={
            withDependents.length > 1
              ? 'Other objects depend on these'
              : 'Other objects depend on this'
          }
        >
          <div className="my-2 max-h-60 space-y-2 overflow-y-auto">
            {withDependents.map((failure, index) => (
              <div key={failure.name ?? index}>
                {failure.name ? (
                  <Text as="div" weight="bold">
                    {failure.name}
                  </Text>
                ) : null}
                <ul className="list-disc pl-5">
                  {failure.objects?.map((object) => (
                    <li key={object} className="break-all">
                      {object}
                    </li>
                  ))}
                </ul>
              </div>
            ))}
          </div>
          <Text as="div">
            Remove them first, or tick <Strong>Enable cascade?</Strong> below
            and try again to remove them as well.
          </Text>
        </IndicatorCard>
      ) : null}
      {otherFailures.length > 0 ? (
        <IndicatorCard
          status="negative"
          showIcon
          className="mt-4"
          headline="Failed for other reasons"
        >
          <div className="my-2 max-h-40 space-y-2 overflow-y-auto">
            {otherFailures.map((failure, index) => (
              <div key={failure.name ?? index}>
                {failure.name ? (
                  <Text as="div" weight="bold">
                    {failure.name}
                  </Text>
                ) : null}
                <Text as="div" className="break-all">
                  {failure.message}
                </Text>
              </div>
            ))}
          </div>
        </IndicatorCard>
      ) : null}
      <div className="mt-4">
        <Checkbox
          value={cascade}
          onChange={(value) => setCascade(value === true)}
        >
          Enable cascade?{' '}
          <IconTooltip message="When set to true, the effect (if possible) is cascaded to any Metadata dependent objects (relationships, permissions, templates)." />
        </Checkbox>
      </div>
    </DestructiveDialog>
  );
};
