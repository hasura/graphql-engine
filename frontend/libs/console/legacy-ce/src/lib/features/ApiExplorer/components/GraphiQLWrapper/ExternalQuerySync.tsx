import { useEffect, useRef } from 'react';
import { useGraphiQL } from '@graphiql/react';
import { planExternalQuerySync } from './graphiqlLifecycle';

/**
 * Syncs an EXTERNAL query (e.g. the `query_file` URL param or a stored query the
 * parent loads asynchronously) into the Monaco editor, without clobbering the
 * user's own edits or tab switches. GraphiQL v5's `initialQuery` prop only seeds
 * the first render, so later external changes are applied imperatively through
 * the store's `queryEditor`.
 *
 * `lastExternal` starts at the mount-time query, so the first effect is a no-op
 * (initialQuery already seeded the editor). The guard
 * (`getValue() !== externalQuery`, in planExternalQuerySync) makes a value the
 * user just typed — which echoes back via onEditQuery -> parent -> this prop — a
 * no-op, so user edits are never overwritten. Must be rendered inside
 * `<GraphiQL>` so the store/provider context exists.
 */
export function ExternalQuerySync({
  externalQuery,
}: {
  externalQuery: string;
}) {
  const queryEditor = useGraphiQL((state) => state.queryEditor);
  const lastExternal = useRef(externalQuery);

  useEffect(() => {
    const plan = planExternalQuerySync({
      externalQuery,
      lastExternal: lastExternal.current,
      editorValue: queryEditor ? queryEditor.getValue() : undefined,
    });
    if (plan.setValue && queryEditor) {
      queryEditor.setValue(plan.value ?? '');
    }
    if (plan.nextLast !== undefined) {
      lastExternal.current = plan.nextLast;
    }
  }, [externalQuery, queryEditor]);

  return null;
}
