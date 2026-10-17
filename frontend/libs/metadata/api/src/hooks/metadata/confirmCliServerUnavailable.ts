import { hasuraToast, useHasuraAlert } from '@hasura/shared/ui';

export const CLI_SERVER_CONFIRM_TEXT = 'CONFIRM';

type HasuraPrompt = ReturnType<typeof useHasuraAlert>['hasuraPrompt'];

/**
 * In-app replacement for the old
 * `getConfirmation('The CLI server is unable to access...', hardConfirmation)`
 * gate that was shown (as a native `prompt`) when the CLI server is unreachable
 * during a migration / raw-SQL run.
 *
 * It preserves the original typed-"CONFIRM" protection: the user must type
 * `CONFIRM` to proceed. It resolves `true` only when the user types the exact
 * confirmation text and accepts, and `false` on cancel or on a non-matching
 * entry (so the caller performs no SQL/migration API call). It never rejects,
 * so it can be awaited safely inside a React Query `mutationFn`.
 *
 * Call `useHasuraAlert()` at the top level of the hook and pass its
 * `hasuraPrompt` here; the returned callback may be invoked later (e.g. inside
 * a `mutationFn`), which is allowed by the Rules of Hooks.
 */
export const confirmCliServerUnavailable = (
  hasuraPrompt: HasuraPrompt,
): Promise<boolean> =>
  new Promise<boolean>((resolve) => {
    hasuraPrompt({
      title: 'CLI server unavailable',
      message: 'The CLI server is unable to access. Do you want to continue?',
      destructive: true,
      confirmText: 'Continue',
      promptLabel: `Type ${CLI_SERVER_CONFIRM_TEXT} to continue.`,
      onCloseAsync: async (result) => {
        if (!result.confirmed) {
          resolve(false);
          return { withSuccess: false };
        }

        if (result.promptValue !== CLI_SERVER_CONFIRM_TEXT) {
          hasuraToast({
            type: 'error',
            title: 'Entry Not Matching',
            message: `Your entry "${result.promptValue}" does not match "${CLI_SERVER_CONFIRM_TEXT}".`,
          });
          resolve(false);
          return { withSuccess: false };
        }

        resolve(true);
        return { withSuccess: false };
      },
    });
  });
