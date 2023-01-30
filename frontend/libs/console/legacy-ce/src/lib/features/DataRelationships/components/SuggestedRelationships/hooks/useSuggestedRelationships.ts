import { DataTarget, useTableRelationships } from '@/features/Datasources';
import { useLocalRelationships } from '@/features/MetadataAPI';

interface TableRelationshipCoreData {
  type: 'object' | 'array';
  from: {
    table: string;
    column: string[];
  };
  to: {
    table: string;
    column: string[];
  };
}

export type ForeignKeyConstraints = ReturnType<
  typeof useTableRelationships
>['data'];
export type ExistingRelationships = ReturnType<
  typeof useLocalRelationships
>['data'];

interface RemoveExistingRelationshipsArgs {
  target: DataTarget;
  foreignKeyConstraints: ForeignKeyConstraints;
  allExistingRelationships: ExistingRelationships;
}

export const removeExistingRelationships = ({
  target,
  foreignKeyConstraints,
  allExistingRelationships,
}: RemoveExistingRelationshipsArgs) => {
  return foreignKeyConstraints?.reduce<TableRelationshipCoreData[]>(
    (acc, { from, to }) => {
      const relationshipExists = allExistingRelationships.find(existing => {
        const fromTableIsReference =
          from?.table === existing?.referenceTable &&
          to?.table === existing?.targetTable;

        const toTableIsReference =
          to?.table === existing?.referenceTable &&
          from?.table === existing?.targetTable;

        return fromTableIsReference || toTableIsReference;
      });

      if (!relationshipExists) {
        // if the table passed into the function is the from table,
        // this means it is on object relationship
        const type: 'object' | 'array' =
          from?.table === target.table ? 'object' : 'array';

        // the `from` and `to` reference the foreign key constraint
        // therefore if the relationship type is array they need to be swapped
        // for example if the foreign key relationship is product.fk_user_id -> user.id
        // it would be listed as from: product, to: user
        // but the suggested relationship should be type: array, from: user, to: product
        if (type === 'object') {
          const suggestedRelationship = { type, from, to };
          acc.push(suggestedRelationship);
        } else {
          const suggestedRelationship = { type, from: to, to: from };
          acc.push(suggestedRelationship);
        }
      }

      return acc;
    },
    []
  );
};

export const useSuggestedRelationships = (target: DataTarget) => {
  const {
    data: foreignKeyConstraints,
    isLoading: foreignKeyConstraintsIsLoading,
    isSuccess: foreignKeyConstraintsIsSuccess,
    isError: foreignKeyConstraintsIsError,
  } = useTableRelationships({
    target,
  });

  const {
    data: allExistingRelationships,
    isLoading: allExistingRelationshipsIsLoading,
    isSuccess: allExistingRelationshipsIsSuccess,
    isError: allExistingRelationshipsIsError,
  } = useLocalRelationships(target);

  const suggestedRelationships = removeExistingRelationships({
    target,
    foreignKeyConstraints,
    allExistingRelationships,
  });

  const isLoading =
    foreignKeyConstraintsIsLoading || allExistingRelationshipsIsLoading;
  const isSuccess =
    foreignKeyConstraintsIsSuccess && allExistingRelationshipsIsSuccess;
  const isError =
    foreignKeyConstraintsIsError || allExistingRelationshipsIsError;

  return { data: suggestedRelationships, isSuccess, isLoading, isError };
};
