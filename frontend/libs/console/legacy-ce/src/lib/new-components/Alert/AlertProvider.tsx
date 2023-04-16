import capitalize from 'lodash/capitalize';
import React, { createContext } from 'react';
import { hasuraToast } from '../Toasts';
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

export const AlertProvider: React.FC = ({ children }) => {
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
    [closeAndCleanup]
  );

  // ideally errors should be handled elsewhere
  // this is a failsafe to prevent an error state from freezing an alert on screen
  const handleUnhandledError = React.useCallback(
    (e: unknown, mode: AlertMode) => {
      setLoading(false);
      closeAndCleanup();
      hasuraToast({
        type: 'error',
        title: `Unhandled error occurred while executing ${mode} dialog.`,
        message: e?.toString() ?? JSON.stringify(e),
      });
    },
    [closeAndCleanup]
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

        // eslint-disable-next-line @typescript-eslint/no-explicit-any
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

        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        extendedProps.onCloseAsync = async (args: any) => {
          setLoading(true);

          try {
            const result = await params.onCloseAsync?.(args);

            setLoading(false);

            dismissAlert({
              withSuccess: result?.withSuccess,
              successText: result?.successText,
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
    [closeAndCleanup, dismissAlert]
  );

  return (
    <AlertContext.Provider
      value={{
        hasuraAlert: params => fireAlert({ ...params, mode: 'alert' }),
        hasuraConfirm: params => fireAlert({ ...params, mode: 'confirm' }),
        hasuraPrompt: params => fireAlert({ ...params, mode: 'prompt' }),
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

const useDestructiveConfirm = () => {
  const { hasuraConfirm } = useHasuraAlert();
  return ({
    resourceName,
    resourceType,
    destroyTerm = 'remove',
    onConfirm,
  }: {
    resourceName: string;
    resourceType: string;
    destroyTerm?: 'delete' | 'remove';
    onConfirm: () => Promise<boolean>;
  }) => {
    if (!onConfirm) throw new Error('onCloseAsync() is required.');

    hasuraConfirm({
      title: `${capitalize(destroyTerm)} ${resourceType}`,
      message: (
        <div>
          Are you sure you want to {destroyTerm} {resourceType}:{' '}
          <strong>{resourceName}</strong>?
        </div>
      ),
      confirmText: 'Remove',
      destructive: true,
      onCloseAsync: async ({ confirmed }) => {
        if (!confirmed) return;

        const success = await onConfirm();

        if (success) {
          return {
            withSuccess: success,
            successText: `${capitalize(destroyTerm)}d ${resourceName}`,
          };
        } else {
          return;
        }
      },
    });
  };
};

const useDestructivePrompt = () => {
  const { hasuraPrompt } = useHasuraAlert();

  return ({
    resourceName,
    resourceType,
    destroyTerm = 'remove',
    onConfirm,
  }: {
    resourceName: string;
    resourceType: string;
    destroyTerm?: 'delete' | 'remove';
    onConfirm: () => Promise<boolean>;
  }) => {
    if (!onConfirm) throw new Error('onCloseAsync() is required.');

    hasuraPrompt({
      title: `${capitalize(destroyTerm)} ${resourceType}`,
      message: (
        <div>
          Are you sure you want to {destroyTerm} {resourceType}:{' '}
          <strong>{resourceName}</strong>?
        </div>
      ),
      confirmText: 'Remove',
      destructive: true,
      promptLabel: (
        <div>
          Type <strong>{resourceName}</strong> to confirm this action.
        </div>
      ),
      onCloseAsync: async result => {
        if (!result.confirmed) return;
        if (result.promptValue !== resourceName) {
          hasuraToast({
            type: 'error',
            title: `Entry Not Matching`,
            message: `Your entry "${result.promptValue}" does not match "${resourceName}".`,
          });
          return;
        } else {
          const success = await onConfirm();

          if (success) {
            return {
              withSuccess: success,
              successText: `${capitalize(destroyTerm)}d ${resourceName}`,
            };
          } else {
            return;
          }
        }
      },
    });
  };
};

export const useDestructiveAlert = () => {
  const destructiveConfirm = useDestructiveConfirm();
  const destructivePrompt = useDestructivePrompt();
  return {
    destructiveConfirm,
    destructivePrompt,
  };
};
