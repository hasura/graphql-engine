import * as AlertDialog from '@radix-ui/react-alert-dialog';
import clsx from 'clsx';
import React from 'react';
import { BsCheckCircleFill } from 'react-icons/bs';
import useUpdateEffect from '../../hooks/useUpdateEffect';
import { sanitizeGraphQLFieldNames } from '../../utils';
import { Button } from '../Button';
import { Input } from '../Form';
import { AlertComponentProps } from './component-types';

const buttonMode = (props: AlertComponentProps) => {
  return props.success
    ? 'success'
    : props.mode !== 'alert' && props.destructive
    ? 'destructive'
    : 'primary';
};

function Buttons(props: AlertComponentProps & { promptValue: string }) {
  const {
    confirmText,
    onClose,
    onCloseAsync,
    promptValue,
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
            autoFocus
            disabled={isLoading}
            onClick={() => {
              if (mode === 'prompt') {
                (onClose ?? onCloseAsync)?.({ confirmed: false });
              } else {
                (onClose ?? onCloseAsync)?.({ confirmed: false });
              }
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
          className={clsx(success && 'pointer-events-none select-none')}
          onClick={() => {
            // pointer-events-none should handle this, but just in case...
            if (success) return;

            if (mode === 'prompt') {
              (onClose ?? onCloseAsync)?.({
                confirmed: true,
                promptValue,
              });
            } else {
              (onClose ?? onCloseAsync)?.({ confirmed: true });
            }
          }}
          iconPosition="end"
          icon={success ? <BsCheckCircleFill /> : undefined}
          mode={buttonMode(props)}
          isLoading={isLoading}
        >
          {confirmText ?? 'Ok'}
        </Button>
      </AlertDialog.Action>
    </div>
  );
}

export const Alert = (props: AlertComponentProps) => {
  const { title, message, mode, open, isLoading, success } = props;
  // we only want to apply an open prop if it's supplied as true/false, but NOT undefined. which likely means the trigger element is in use.
  const openProps = React.useMemo(
    () => ({
      ...(typeof open !== 'undefined' && { open: open }),
    }),
    [open]
  );

  // makes sure the prompt values are cleared after close
  useUpdateEffect(() => {
    if (open === false) {
      setPromptValue('');
    }
  }, [open]);

  const [promptValue, setPromptValue] = React.useState('');
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
          {mode === 'prompt' && (
            <div className="mb-5">
              {!!props.promptLabel && (
                <label
                  className={clsx('block pt-1 text-muted mb-1')}
                  htmlFor="prompt-input"
                >
                  {props.promptLabel}
                </label>
              )}
              <Input
                disabled={isLoading || success}
                placeholder={props?.promptPlaceholder ?? ''}
                name="prompt-input"
                //disabled={isLoading || success}
                fieldProps={{ value: promptValue, autoFocus: true }}
                onChange={e => {
                  let value = e.target.value;

                  if (props.sanitizeGraphQL) {
                    value = sanitizeGraphQLFieldNames(e.target.value);
                  }

                  setPromptValue(value);
                }}
              />
            </div>
          )}
          <Buttons {...props} promptValue={promptValue} />
        </AlertDialog.Content>
      </AlertDialog.Portal>
    </AlertDialog.Root>
  );
};
