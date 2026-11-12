import { isObject } from '@hasura/shared/utils';

/**
 * Router state of the Insert Row page that prefills the form with a row to
 * clone, e.g. `navigate(insertPath, { state: toCloneRowState(row) })`.
 */
type CloneRowState = { cloneRow: Record<string, unknown> };

export const toCloneRowState = (
  row: Record<string, unknown>,
): CloneRowState => ({ cloneRow: row });

/** Reads the row to clone from the router state, if any. */
export const getCloneRow = (
  state: unknown,
): Record<string, unknown> | undefined =>
  isObject(state) && isObject((state as Partial<CloneRowState>).cloneRow)
    ? (state as CloneRowState).cloneRow
    : undefined;
