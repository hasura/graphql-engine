import { Fragment, ReactNode } from 'react';
import get from 'lodash/get';
import { TableProvider } from '../TableProvider';
import { PermissionType, Relationships } from '../types';

export function createWrapper({
  types,
  path,
  relationships,
}: {
  types: Record<
    string,
    {
      type: PermissionType;
    }
  >;
  path: string[];
  relationships: Relationships;
}) {
  let Wrapper: any = Fragment;
  const type = get(types, path)?.type;

  if (type === 'relationship') {
    const relationship = relationships.find(
      r => r.name === path[path.length - 1]
    );
    if (relationship) {
      if (!('toTable' in relationship.definition)) {
        return;
      }
      const relationshipTable = relationship.definition.toTable;
      Wrapper = ({ children }: { children?: ReactNode | undefined }) => (
        <TableProvider table={relationshipTable}>{children}</TableProvider>
      );
    }
  }
  return Wrapper;
}
