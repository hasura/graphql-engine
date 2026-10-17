import { capitalize } from 'inflection';
import React, { createContext } from 'react';
import { hasuraToast, showErrorNotification } from '../Toasts';
import { Alert } from './Alert';
import { AlertComponentProps } from './component-types';
import {
  AlertMode,
  AlertParams,
  ConfirmParams,
  DismissAlertParams,
  Params,
  PromptParams,
} from './types';
import { Text } from '../typography';
import { Strong } from '@radix-ui/themes';

interface AlertContextType {
  hasuraAlert: (props: Params<'alert'>) => void;
  hasuraConfirm: (props: Params<'confirm'>) => void;
  hasuraPrompt: (props: Params<'prompt'>) => void;
}

const AlertContext = createContext<AlertContextType>({
  hasuraAlert: async () => {
    // init
  },
  hasuraConfirm: async () => {
    // init
  },
  hasuraPrompt: async () => {
    // init
  },
});

export const useHasuraAlert = () => {
  const ctx = React.useContext(AlertContext);

  return ctx;
};

export const AlertProvider = ({ children }: { children: React.ReactNode }) => {
  const [showAlert, setShowAlert] = React.useState(false);

  const [componentProps, setComponentProps] =
    React.useState<AlertComponentProps | null>(null);
  const [loading, setLoading] = React.useState(false);
  const [success, setSuccess] = React.useState(false);
  const [successText, setSuccessText] = React.useState<string | null>(null);

  const closeAndCleanup = React.useCallback(() => {
    setShowAlert(false);

    //reset success state
    setSuccess(false);
    setSuccessText(null);
    setLoading(false);
    setComponentProps(null);
  }, []);

  const dismissAlert = React.useCallback(
    (props?: DismissAlertParams) => {
      const { withSuccess, successText, successDelay = 1500 } = props ?? {};

      if (withSuccess) {
        if (successText) {
          setSuccessText(successText);
        }

        //show a success state
        setSuccess(true);

        setTimeout(() => {
          closeAndCleanup();
        }, successDelay);
      } else {
        closeAndCleanup();
      }
    },
    [closeAndCleanup],
  );

  // ideally errors should be handled elsewhere
  // this is a failsafe to prevent an error state from freezing an alert on screen
  const handleUnhandledError = React.useCallback(
    (e: unknown, mode: AlertMode) => {
      setLoading(false);
      closeAndCleanup();
      showErrorNotification({
        title: `Unhandled error occurred while executing ${mode} dialog.`,
        error: e,
      });
    },
    [closeAndCleanup],
  );

  // function that will fire an alert:
  const fireAlert = React.useCallback(
    (params: AlertParams | ConfirmParams | PromptParams) => {
      // just in case so we don't display a broken alert
      if (!params) throw Error('Invalid state passed to fireAlert()');

      const extendedProps = { ...params };

      if (
        extendedProps?.onClose ||
        //handle when alert has no onClose or onCloseAsync passed
        (params.mode === 'alert' && !params?.onClose && !params?.onCloseAsync)
      ) {
        //SYNC MODE:
        extendedProps.onClose = async (args: any) => {
          closeAndCleanup();
          // call original callback
          try {
            params.onClose?.(args);
          } catch (e) {
            handleUnhandledError(e, params.mode);
          }
        };
      } else {
        //ASYNC MODE:
        extendedProps.onCloseAsync = async (args: any) => {
          setLoading(true);

          try {
            const result = await params.onCloseAsync?.(args);

            setLoading(false);

            if (!result) {
              return;
            }

            dismissAlert({
              withSuccess: result.withSuccess,
              successText: result.successText,
            });
          } catch (e) {
            handleUnhandledError(e, params.mode);
          }
        };
      }

      // this is to prevent an issue where if moving from other radix components that involve overlays, it will get confused and leave the overlay pointer-events:none property on the body.
      // the timeout prevents overlapping css calls to modify the pointer events on the body
      setTimeout(() => {
        setComponentProps({ ...extendedProps });
        setShowAlert(true);
      }, 0);
    },
    [closeAndCleanup, dismissAlert, handleUnhandledError],
  );

  const hasuraAlert: AlertContextType['hasuraAlert'] = React.useCallback(
    (params) => fireAlert({ ...params, mode: 'alert' }),
    [fireAlert],
  );
  const hasuraConfirm: AlertContextType['hasuraConfirm'] = React.useCallback(
    (params) => fireAlert({ ...params, mode: 'confirm' }),
    [fireAlert],
  );
  const hasuraPrompt: AlertContextType['hasuraPrompt'] = React.useCallback(
    (params) => fireAlert({ ...params, mode: 'prompt' }),
    [fireAlert],
  );

  return (
    <AlertContext.Provider
      value={{
        hasuraAlert,
        hasuraConfirm,
        hasuraPrompt,
        //use these to test instable references:
        //   hasuraAlert: params => fireAlert({ ...params, mode: 'alert' }),
        //   hasuraConfirm: params => fireAlert({ ...params, mode: 'confirm' }),
        //   hasuraPrompt: params => fireAlert({ ...params, mode: 'prompt' }),
      }}
    >
      {children}
      <Alert
        {...(componentProps ?? {
          message: 'error',
          title: 'error',
          mode: 'alert',
          onClose: () => {
            // init
          },
        })}
        //this ensures the component is given a new instances when it's closed/re-opened
        key={showAlert.toString()}
        isLoading={loading}
        open={showAlert}
        success={success}
        confirmText={
          success && !!successText ? successText : componentProps?.confirmText
        }
      />
    </AlertContext.Provider>
  );
};

/**
 *
 * These are wrapper hooks that simplify and standardize the API/UI/UX for remove/delete operations
 *
 */
export type DestructiveConfirmArgs = {
  resourceName: string;
  resourceType: string;
  destroyTerm?: 'delete' | 'remove';
  appendTerm?: string;
  children?: React.ReactNode;
  onConfirm: () => Promise<boolean>;
};

export const useDestructiveConfirm = () => {
  const { hasuraConfirm } = useHasuraAlert();
  // Returns a promise that settles when the dialog closes: `true` once the
  // destructive `onConfirm` has run and reported success, `false` on cancel,
  // on `onConfirm` returning falsy, or if `onConfirm` throws. Callers that only
  // want to fire-and-forget can ignore the returned promise.
  return React.useCallback(
    ({
      resourceName,
      resourceType,
      destroyTerm = 'remove',
      appendTerm = '',
      onConfirm,
      children,
    }: DestructiveConfirmArgs): Promise<boolean> =>
      new Promise<boolean>((resolve) => {
        hasuraConfirm({
          title: `${capitalize(destroyTerm)} ${resourceType}`,
          message: (
            <div className="py-4">
              <Text>
                Are you sure you want to {destroyTerm} {resourceType}:{' '}
                <Strong>{resourceName}</Strong>?
                {appendTerm ? ` ${appendTerm}` : ''}
              </Text>
              {children}
            </div>
          ),
          confirmText: 'Remove',
          destructive: true,
          onCloseAsync: async ({ confirmed }) => {
            if (!confirmed) {
              resolve(false);
              return {
                withSuccess: false,
              };
            }

            try {
              const success = await onConfirm();
              resolve(success);

              if (success) {
                return {
                  withSuccess: success,
                  successText: `${capitalize(destroyTerm)}d ${resourceName}`,
                };
              }

              return {
                withSuccess: false,
              };
            } catch (e) {
              // settle the returned promise, then let AlertProvider surface the
              // error (its failsafe toast) instead of leaving the alert frozen.
              resolve(false);
              throw e;
            }
          },
        });
      }),
    [hasuraConfirm],
  );
};

export const useDestructivePrompt = () => {
  const { hasuraPrompt } = useHasuraAlert();

  // Returns a promise that settles when the dialog closes: `true` once the
  // typed name matched and `onConfirm` reported success, `false` on cancel, on
  // a non-matching typed name, on `onConfirm` returning falsy, or if `onConfirm`
  // throws. Callers that only want to fire-and-forget can ignore the promise.
  return React.useCallback(
    ({
      resourceName,
      resourceType,
      destroyTerm = 'remove',
      appendTerm = '',
      onConfirm,
    }: {
      resourceName: string;
      resourceType: string;
      destroyTerm?: 'delete' | 'remove';
      appendTerm?: string;
      onConfirm: () => Promise<boolean>;
    }): Promise<boolean> =>
      new Promise<boolean>((resolve) => {
        hasuraPrompt({
          title: `${capitalize(destroyTerm)} ${resourceType}`,
          message: (
            <div className="py-4">
              Are you sure you want to {destroyTerm} {resourceType}:{' '}
              <strong>{resourceName}</strong>?
              {appendTerm ? ` ${appendTerm}` : ''}
            </div>
          ),
          confirmText: 'Remove',
          destructive: true,
          promptLabel: (
            <div className="py-4">
              Type <strong>{resourceName}</strong> to confirm this action.
            </div>
          ),
          onCloseAsync: async (result) => {
            if (!result.confirmed) {
              resolve(false);
              return {
                withSuccess: false,
              };
            }

            if (result.promptValue !== resourceName) {
              hasuraToast({
                type: 'error',
                title: `Entry Not Matching`,
                message: `Your entry "${result.promptValue}" does not match "${resourceName}".`,
              });
              resolve(false);
              return {
                withSuccess: false,
              };
            }

            try {
              const success = await onConfirm();
              resolve(success);

              if (success) {
                return {
                  withSuccess: success,
                  successText: `${capitalize(destroyTerm)}d ${resourceName}`,
                };
              }

              return {
                withSuccess: false,
              };
            } catch (e) {
              resolve(false);
              throw e;
            }
          },
        });
      }),
    [hasuraPrompt],
  );
};

export const useDestructiveAlert = () => {
  const destructiveConfirm = useDestructiveConfirm();
  const destructivePrompt = useDestructivePrompt();
  return {
    destructiveConfirm,
    destructivePrompt,
  };
};
