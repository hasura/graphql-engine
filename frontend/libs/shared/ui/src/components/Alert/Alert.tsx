import { AlertDialog } from '@radix-ui/themes';
import clsx from 'clsx';
import React from 'react';
import { BsCheckCircleFill } from 'react-icons/bs';
import { z } from 'zod';
import { reqString } from '@hasura/shared/utils';
import { Button } from '../Button';
import {
  GraphQLSanitizedInputField,
  InputField,
  useConsoleForm,
} from '../Form';
import { AlertComponentProps } from './component-types';

const buttonMode = (props: AlertComponentProps) => {
  return props.success
    ? 'success'
    : props.mode !== 'alert' && props.destructive
      ? 'destructive'
      : 'primary';
};

function Buttons(props: AlertComponentProps) {
  const { confirmText, onClose, onCloseAsync, mode, isLoading, success } =
    props;

  return (
    <div className="flex justify-end gap-[12px]">
      {(mode === 'confirm' || mode === 'prompt') && !success && (
        <AlertDialog.Cancel
          onClick={() => {
            (onClose ?? onCloseAsync)?.({ confirmed: false });
          }}
        >
          {/* CANCEL BUTTON: */}
          <Button mode="default" disabled={isLoading}>
            {props?.cancelText ?? 'Cancel'}
          </Button>
        </AlertDialog.Cancel>
      )}
      <AlertDialog.Action
        onClick={() => {
          // pointer-events-none should handle this, but just in case...
          if (success) return;

          //prompt is handled in the form submission
          if (mode !== 'prompt') {
            (onClose ?? onCloseAsync)?.({ confirmed: true, promptValue: '' });
          }
        }}
      >
        {/* CONFIRM BUTTON: */}
        <Button
          autoFocus
          disabled={isLoading}
          type={mode === 'prompt' ? 'submit' : 'button'}
          className={clsx(success && 'pointer-events-none select-none')}
          rightIcon={success ? BsCheckCircleFill : undefined}
          mode={buttonMode(props)}
          loading={isLoading}
          data-testid={'alert-confirm-button'}
        >
          {confirmText ?? 'Ok'}
        </Button>
      </AlertDialog.Action>
    </div>
  );
}

export const Alert = (props: AlertComponentProps) => {
  const {
    title,
    message,
    mode,
    open,
    isLoading,
    success,
    onClose,
    onCloseAsync,
  } = props;
  const defaultInputValue =
    mode === 'prompt' && props?.defaultValue ? props.defaultValue : '';
  const inputFieldName =
    mode === 'prompt' && props?.inputFieldName ? props.inputFieldName : 'value';

  const inputId = 'prompt_value' as const;

  const { Form } = useConsoleForm({
    schema: z.object({ [inputId]: reqString(inputFieldName) }),
    options: {
      mode: 'all',
      defaultValues: {
        [inputId]: defaultInputValue,
      },
    },
  });

  return (
    <AlertDialog.Root open={open}>
      <AlertDialog.Content maxWidth="500px">
        <AlertDialog.Title>{title}</AlertDialog.Title>
        <AlertDialog.Description
          className={clsx(mode === 'prompt' ? 'mb-3' : 'mb-5')}
        >
          {message}
        </AlertDialog.Description>
        <Form
          onSubmit={(values) => {
            (onClose ?? onCloseAsync)?.({
              confirmed: true,
              promptValue: values[inputId],
            });
          }}
        >
          {mode === 'prompt' && (
            <div className="mb-5">
              {!!props.promptLabel && (
                <label
                  className={clsx('block pt-1 text-muted mb-1')}
                  htmlFor={inputId}
                >
                  {props.promptLabel}
                </label>
              )}

              {props.sanitizeGraphQL ? (
                <GraphQLSanitizedInputField
                  name={inputId}
                  fieldProps={{
                    disabled: isLoading || success,
                    placeholder: props?.promptPlaceholder ?? '',
                  }}
                />
              ) : (
                <InputField
                  name={inputId}
                  fieldProps={{
                    disabled: isLoading || success,
                    placeholder: props?.promptPlaceholder ?? '',
                  }}
                />
              )}
            </div>
          )}
          <Buttons
            {...props}
            onClose={onClose as any}
            onCloseAsync={onCloseAsync as any}
          />
        </Form>
      </AlertDialog.Content>
    </AlertDialog.Root>
  );
};
