import { isLogicalModelType } from '../../../hasura-metadata-types';
import { getQualifiedTable } from '../../ManageTable/utils';
import { findReferencedEntities } from './utils/findReferencedEntities';

export const DisplayReferencedLogicalModelEntities = ({
  entities: { stored_procedures, native_queries, logical_models, tables },
}: {
  entities: ReturnType<typeof findReferencedEntities>;
}) => {
  return (
    <div>
      {tables.length > 0 && (
        <div className="mt-2">
          <strong className="mb-2">Tables:</strong>
          {tables.map(t => (
            <pre className="ml-2">
              - {getQualifiedTable(t.table).join(' / ')}
            </pre>
          ))}
        </div>
      )}
      {native_queries.length > 0 && (
        <div className="mt-2">
          <strong className="mb-2">Native Queries:</strong>
          {native_queries.map(q => (
            <pre className="ml-2"> - {q.root_field_name}</pre>
          ))}
        </div>
      )}
      {stored_procedures.length > 0 && (
        <div className="mt-2">
          <strong className="mb-2">Stored Procedures:</strong>
          {stored_procedures.map(q => (
            <pre className="ml-2 whitespace-nowrap">
              {/* since the 'stored_procedure' property is type `unknown` going to do this to be safe so we don't throw errors until the type is more specific: */}
              - {JSON.stringify(q.stored_procedure, null, 2)}
            </pre>
          ))}
        </div>
      )}
      {logical_models.length > 0 && (
        <div className="mt-2">
          <strong className="mb-2">Logical Models:</strong>
          {logical_models.map(m => (
            <pre className="ml-2">
              {` - `}
              {m.logicalModel.name} (
              {m.matchingFields
                .map(f =>
                  // it's either an array type or not, so if it's an array, add the `[]` string to show the difference.
                  isLogicalModelType(f.type) ? f.name : f.name + '[]'
                )
                .join(', ')}
              )
            </pre>
          ))}
        </div>
      )}
    </div>
  );
};
