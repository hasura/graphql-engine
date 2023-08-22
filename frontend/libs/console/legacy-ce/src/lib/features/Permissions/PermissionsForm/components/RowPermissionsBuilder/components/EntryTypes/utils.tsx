import { Fragment, ReactNode } from 'react';
import get from 'lodash/get';
import { TableProvider } from '../TableProvider';
import { PermissionType, Relationships } from '../types';

export function Wrapper({
  children,
  types,
  path,
  relationships,
}: {
  children?: ReactNode;
  types: Record<
    string,
    {
      type: PermissionType;
    }
  >;
  path: string[];
  relationships: Relationships;
}) {
  const type = get(types, path)?.type;

  // MongoDB's nested objects behave like relationships
  // so we put them inside a wrapper that has their fields as columns
  if (type === 'object') {
    return <TableProvider>{children}</TableProvider>;
  }

  if (type === 'relationship') {
    const relationship = relationships.find(
      r => r.name === path[path.length - 1]
    );
    if (relationship) {
      if (!('toTable' in relationship.definition)) {
        return null;
      }
      const relationshipTable = relationship.definition.toTable;
      return (
        <TableProvider table={relationshipTable}>{children}</TableProvider>
      );
    }
  }

  return <Fragment>{children}</Fragment>;
}
