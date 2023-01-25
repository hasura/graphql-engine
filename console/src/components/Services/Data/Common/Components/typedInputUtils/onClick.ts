/**
 * At the time of writing, the goal was just to fix the Sentry CONSOLE-6J and Console-6E issues.
 * Because onClick is spread across a long variety of components, even identifying its correct type
 * was hard. Based on the fact that onClick is also passed to a select Element, I was able to identify
 * the event. Anyway, the SimplifiedClickEvent is a best guess that also allow us passing almost
 * whatever from the unit tests.
 *
 * @see: https://sentry.io/organizations/hasura-nn/issues/3679294754/activity/?project=6684052
 * @see: https://sentry.io/organizations/hasura-nn/issues/3679762841/activity/?project=6684052
 */
type SimplifiedClickEvent = { target: React.MouseEvent['target'] };

export function onClick(e?: SimplifiedClickEvent) {
  if (!(e?.target instanceof HTMLElement)) return;

  const closestRadio = e.target.closest?.('.radio-inline');
  if (closestRadio) {
    const radioButton = closestRadio.querySelector<HTMLInputElement>(
      'input[type="radio"]'
    );

    if (radioButton) {
      radioButton.checked = true;
    }
  }

  e.target.focus?.();
}
