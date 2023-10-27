import * as AlertDialog from '@radix-ui/react-alert-dialog';
import clsx from 'clsx';
import React from 'react';
import { BsCheckCircleFill } from 'react-icons/bs';
import { z } from 'zod';
import { reqString } from '../../utils/zodUtils';
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
  const {
    confirmText,
    onClose,
    onCloseAsync,

    mode,
    isLoading,
    success,
  } = props;

  return (
    <div className="flex justify-end gap-[12px]">
      {(mode === 'confirm' || mode === 'prompt') && !success && (
        <AlertDialog.Cancel asChild>
          {/* CANCEL BUTTON: */}
          <Button
            type={'reset'}
            autoFocus
            disabled={isLoading}
            onClick={() => {
              (onClose ?? onCloseAsync)?.({ confirmed: false });
            }}
          >
            {props?.cancelText ?? 'Cancel'}
          </Button>
        </AlertDialog.Cancel>
      )}
      <AlertDialog.Action asChild>
        {/* CONFIRM BUTTON: */}
        <Button
          autoFocus
          disabled={isLoading}
          type={mode === 'prompt' ? 'submit' : 'button'}
          className={clsx(success && 'pointer-events-none select-none')}
          onClick={() => {
            // pointer-events-none should handle this, but just in case...
            if (success) return;

            //prompt is handled in the form submission
            if (mode !== 'prompt') {
              (onClose ?? onCloseAsync)?.({ confirmed: true });
            }
          }}
          iconPosition="end"
          icon={success ? <BsCheckCircleFill /> : undefined}
          mode={buttonMode(props)}
          isLoading={isLoading}
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
  // we only want to apply an open prop if it's supplied as true/false, but NOT undefined. which likely means the trigger element is in use.
  const openProps = React.useMemo(
    () => ({
      ...(typeof open !== 'undefined' && { open: open }),
    }),
    [open]
  );

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
    <AlertDialog.Root {...openProps}>
      <AlertDialog.Portal>
        <AlertDialog.Overlay className="bg-gray-700/40 z-[100] data-[state=open]:animate-fadeIn fixed inset-0" />
        <AlertDialog.Content className="z-[101] data-[state=open]:animate-alertContentShow fixed top-[50%] left-[50%] max-h-[85vh] w-[90vw] max-w-[500px] translate-x-[-50%] translate-y-[-50%] rounded-[6px] bg-white p-[25px] shadow-[hsl(206_22%_7%_/_35%)_0px_10px_38px_-10px,_hsl(206_22%_7%_/_20%)_0px_10px_20px_-15px] focus:outline-none">
          <AlertDialog.Title className="text-gray-900 m-0 text-[17px] font-medium">
            {title}
          </AlertDialog.Title>
          <AlertDialog.Description
            className={clsx(
              'text-gray-700 mt-4  text-[15px] leading-normal',
              mode === 'prompt' ? 'mb-3' : 'mb-5'
            )}
          >
            {message}
          </AlertDialog.Description>
          <Form
            onSubmit={values => {
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
                    disabled={isLoading || success}
                    placeholder={props?.promptPlaceholder ?? ''}
                    name={inputId}
                  />
                ) : (
                  <InputField
                    disabled={isLoading || success}
                    placeholder={props?.promptPlaceholder ?? ''}
                    name={inputId}
                  />
                )}
              </div>
            )}
            <Buttons {...props} />
          </Form>
        </AlertDialog.Content>
      </AlertDialog.Portal>
    </AlertDialog.Root>
  );
};
