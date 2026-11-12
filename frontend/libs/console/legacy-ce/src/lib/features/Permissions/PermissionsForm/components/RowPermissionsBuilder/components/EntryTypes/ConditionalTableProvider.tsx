import { Fragment, ReactNode, useContext } from 'react';
import get from 'lodash/get';
import { TableProvider, tableContext } from '../TableProvider';
import { typesContext } from '../TypesProvider';
import { getRelationshipTargetSource } from '../utils/tables';

/**
 * Conditionally wraps children with TableProvider, based on the type of the field
 * - For relationships, it wraps the children with the table of the relationship
 * - For MongoDB nested objects, it wraps the children with the object's fields
 */
export function ConditionalTableProvider({
  children,
  path,
}: {
  children?: ReactNode;
  path: string[];
}) {
  const { types } = useContext(typesContext);
  const { relationships } = useContext(tableContext);
  const type = get(types, path)?.type;

  // MongoDB's nested objects behave like relationships
  // so we put them inside a wrapper that has their fields as columns
  if (type === 'object') {
    return (
      <TableProvider objectPath={path.join('.')}>{children}</TableProvider>
    );
  }

  if (type === 'relationship') {
    const relationship = relationships.find(
      (r) => r.name === path[path.length - 1],
    );
    if (relationship) {
      if (!('toTable' in relationship.definition)) {
        return null;
      }
      const relationshipTable = relationship.definition.toTable;
      const relationshipSource = getRelationshipTargetSource(relationship);
      // the target of a relationship on a foreign key is known once its table
      // is loaded, so the provider restarts with the resolved table
      return (
        <TableProvider
          key={JSON.stringify([relationshipSource, relationshipTable ?? null])}
          table={relationshipTable}
          source={relationshipSource}
          isRemote={relationship.type === 'remoteDatabaseRelationship'}
        >
          {children}
        </TableProvider>
      );
    }
  }

  return <Fragment>{children}</Fragment>;
}
