import {
  Kind,
  OperationDefinitionNode,
  OperationTypeNode,
  print,
} from 'graphql';
import { programmaticallyTraceError } from '@hasura/shared/analytics';

export const getCacheRequestWarning = (
  warningHeader: string | null,
): string | null => {
  if (!warningHeader) {
    return null;
  }

  return [
    'cache-store-size-limit-exceeded',
    'cache-store-capacity-exceeded',
    'cache-store-error',
  ]?.some((warning) => warningHeader?.includes(warning))
    ? warningHeader
    : null;
};

// Simulates the Run button click on the GraphiQL editor
export const clickRunQueryButton = () => {
  const runQueryButton = document.getElementsByClassName(
    'graphiql-execute-button',
  );

  // trigger click
  if (runQueryButton && runQueryButton[0]) {
    (runQueryButton[0] as HTMLButtonElement).click();
  } else {
    const error = new Error(
      'Could not find run query button in the DOM (.graphiql-execute-button)',
    );
    programmaticallyTraceError(error);
  }
};

export const toggleCacheDirective = (
  operations: OperationDefinitionNode[],
  onlyAdd = false,
): string | null => {
  const shouldAddCacheDirective = !operations.some((def) => {
    return def.directives?.some((dir) => dir.name.value === 'cached');
  });

  if (onlyAdd && !shouldAddCacheDirective) {
    return null;
  }

  return operations
    .map((operation) => {
      if (operation.operation !== OperationTypeNode.QUERY) {
        return operation;
      }

      const newDef = {
        ...operation,
        directives:
          operation.directives?.filter((dir) => dir.name.value !== 'cached') ??
          [],
      };

      if (shouldAddCacheDirective) {
        newDef.directives.push({
          kind: Kind.DIRECTIVE,
          name: {
            kind: Kind.NAME,
            value: 'cached',
          },
        });
      }

      return newDef;
    })
    .map((op) => print(op))
    .join('\n\n');
};
