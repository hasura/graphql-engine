import { useCallback } from 'react';
import { useMetadataMigration } from '@hasura/metadata/api';
import { getErrorMessage } from '@hasura/shared/utils';
import { hasuraToast } from '@hasura/shared/ui';
import type {
  CustomTypeObjectRelationship,
  CustomTypes,
} from '@hasura/shared/types';
import { generateSetCustomTypesQuery } from './useSetCustomGraphQLTypes';
import { removeTypeRelationship } from '../utils';
import type { CustomTypeObjectRelationshipFormState } from '../types';

const errorMsg = 'Saving relationship failed';

type AddActionRelationshipArgs = {
  typeName: string;
  existingTypes: CustomTypes;
  existingRelConfig?: CustomTypeObjectRelationship;
  relConfig: CustomTypeObjectRelationshipFormState;
};

const useAddActionRelationship = () => {
  const mutation = useMetadataMigration();

  return useCallback(
    async (
      {
        typeName,
        existingTypes,
        relConfig,
        existingRelConfig,
      }: AddActionRelationshipArgs,
      onSuccess?: () => unknown,
    ) => {
      let typesWithRels: CustomTypes = existingTypes;

      if (existingRelConfig) {
        // modifying existing relationship
        // if the relationship is being renamed
        if (existingRelConfig.name !== relConfig.name) {
          // validate the new name
          const validationError = validateRelationshipTypename(
            existingTypes,
            typeName,
            relConfig.name,
          );
          if (validationError) {
            return hasuraToast({
              type: 'error',
              title: errorMsg,
              message: validationError,
            });
          }

          // remove old relationship from types
          typesWithRels = removeTypeRelationship(
            existingTypes,
            typeName,
            existingRelConfig.name,
          );
        }
      } else {
        // creating a new relationship

        // validate the relationship name
        const validationError = validateRelationshipTypename(
          existingTypes,
          typeName,
          relConfig.name,
        );

        if (validationError) {
          return hasuraToast({
            type: 'error',
            title: errorMsg,
            message: validationError,
          });
        }
      }

      // add modified relationship to types
      typesWithRels = injectTypeRelationship(
        typesWithRels,
        typeName,
        relConfig,
      ) as CustomTypes;

      return mutation.mutate(
        {
          query: generateSetCustomTypesQuery(typesWithRels),
        },
        {
          onSuccess: () => {
            onSuccess?.();
            hasuraToast({
              title: 'Relationship saved successfully!',
              type: 'success',
            });
          },
          onError: (err) => {
            hasuraToast({
              title: errorMsg,
              message: getErrorMessage(err),
              type: 'error',
            });
          },
        },
      );
    },
    [mutation],
  );
};

const validateRelationshipTypename = (
  types: CustomTypes,
  typename: string,
  relname: string,
): string | null => {
  if (
    types.objects?.some(
      (t) =>
        t.name === typename && t.relationships?.some((r) => r.name === relname),
    )
  ) {
    return `Relationship with name "${relname}" already exists.`;
  }

  return null;
};

const injectTypeRelationship = (
  types: CustomTypes,
  typename: string,
  relConfig: CustomTypeObjectRelationshipFormState,
) => {
  return {
    ...types,
    objects: types.objects?.map((t) => {
      if (t.name !== typename) {
        return t;
      }

      return {
        ...t,
        relationships: [
          ...(t.relationships ?? []).filter((r) => r.name !== relConfig.name),
          reformRelationship(relConfig),
        ],
      };
    }),
  };
};

const reformRelationship = (
  relConfig: CustomTypeObjectRelationshipFormState,
) => {
  return {
    ...relConfig,
    field_mapping: relConfig.field_mapping.reduce((all, fm) => {
      if (fm.column && fm.field) {
        return {
          ...all,
          [fm.field]: fm.column,
        };
      }
      return all;
    }, {}),
  };
};

export default useAddActionRelationship;
