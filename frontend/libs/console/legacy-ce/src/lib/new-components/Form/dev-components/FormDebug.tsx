import { useFormContext } from 'react-hook-form';
import { Collapsible } from '../../Collapsible';
import { sessionStore } from '../../../utils/sessionStorage';
import { consoleDevToolsEnabled } from '../../../utils/console-dev-tools/consoleDevToolsEnabled';

/**
 *
 * Render this component inside your form to see a real-time JSON readout of your form values/errors.
 * FormDebugWindow is usually preferrable, but this is handy to render directly for forms inside of dialogs
 *
 */
export const FormDebug = () => {
  const methods = useFormContext();
  const formValues = methods.watch();
  const formState = methods.formState;

  const friendlyErrors = () => {
    try {
      return Object.entries(formState.errors).reduce<
        Record<string, { message: string; type: string }>
      >((result, [key, value]) => {
        return {
          ...result,
          [key]: {
            message: value.message,
            type: value.type,
          },
        };
      }, {});
    } catch {
      return {};
    }
  };

  const tryStringify = (x: unknown) => {
    try {
      return JSON.stringify(x, null, 2);
    } catch (e: any) {
      return `{ ${e?.message || 'error'} }`;
    }
  };

  const devToolsEnabled = consoleDevToolsEnabled();

  if (!devToolsEnabled) return null;

  return (
    <div className="flex flex-col gap-2 right-0 text-sm">
      <Collapsible
        defaultOpen={
          sessionStore.getItem('formDebug.defaultOpen.values') === 'y'
        }
        onOpenChange={open => {
          sessionStore.setItem(
            'formDebug.defaultOpen.values',
            open ? 'y' : 'n'
          );
        }}
        triggerChildren={'Values'}
      >
        <div className="whitespace-pre">{tryStringify(formValues)}</div>
      </Collapsible>
      <Collapsible
        defaultOpen={
          sessionStore.getItem('formDebug.defaultOpen.errors') === 'y'
        }
        onOpenChange={open => {
          sessionStore.setItem(
            'formDebug.defaultOpen.errors',
            open ? 'y' : 'n'
          );
        }}
        triggerChildren={'Errors'}
      >
        <div className="whitespace-pre">{tryStringify(friendlyErrors())}</div>
      </Collapsible>
    </div>
  );
};
