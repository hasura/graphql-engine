import { useState } from 'react';
import { Flex, Strong } from '@radix-ui/themes';
import {
  Button,
  IconButton,
  IndicatorCard,
  SkeletonList,
  Text,
  useDestructiveConfirm,
} from '@hasura/shared/ui';
import { FaPlus, FaTrash } from 'react-icons/fa';
import {
  getDatabaseMethods,
  useCreateCheckConstraint,
  useDropCheckConstraint,
  useTableCheckConstraints,
} from '@hasura/metadata/data-source';
import { ModifyTableProps } from '../types';
import {
  CheckConstraintForm,
  CheckConstraintFormValues,
  emptyCheckConstraintFormValues,
} from './CheckConstraintForm';

/**
 * List / add / remove CHECK constraints on an existing table using real driver
 * methods (Postgres family). Sections are gated by the actual method:
 * `introspection.getCheckConstraints` (list), `modify.createCheckConstraint`
 * (add), `modify.dropCheckConstraint` (remove).
 */
export const CheckConstraints = ({ source, table }: ModifyTableProps) => {
  const dbMethods = getDatabaseMethods(source.kind);
  const canList = Boolean(dbMethods.introspection.getCheckConstraints);
  const canAdd = Boolean(dbMethods.modify?.createCheckConstraint);
  const canDrop = Boolean(dbMethods.modify?.dropCheckConstraint);

  const destructiveConfirm = useDestructiveConfirm();
  const [isAddDialogOpen, setIsAddDialogOpen] = useState(false);

  const {
    data: checkConstraints = [],
    isLoading,
    isError,
  } = useTableCheckConstraints(
    { source, table: table.table },
    { enabled: canList },
  );

  const { mutateAsync: dropCheckConstraint } = useDropCheckConstraint();

  const { mutate: createCheckConstraint, isPending } = useCreateCheckConstraint(
    {
      onSuccess: () => setIsAddDialogOpen(false),
    },
  );

  const onSubmit = (values: CheckConstraintFormValues) => {
    createCheckConstraint({
      source: { name: source.name, kind: source.kind },
      table: table.table,
      constraintName: values.name,
      check: values.check,
    });
  };

  return (
    <div>
      {canList &&
        (isLoading ? (
          <SkeletonList count={2} />
        ) : isError ? (
          <IndicatorCard status="negative" headline="error">
            Unable to fetch check constraints
          </IndicatorCard>
        ) : checkConstraints.length === 0 ? (
          <div className="mb-2">
            <Text>No check constraints found.</Text>
          </div>
        ) : (
          <div className="mb-2">
            {checkConstraints.map((constraint) => (
              <Flex
                key={constraint.name}
                align="center"
                gap="2"
                className="mb-2"
              >
                {canDrop && (
                  <IconButton
                    type="button"
                    mode="destructive"
                    variant="outline"
                    size="1"
                    icon={FaTrash}
                    title={`Remove check constraint ${constraint.name}`}
                    onClick={() =>
                      destructiveConfirm({
                        resourceName: constraint.name,
                        resourceType: 'Check Constraint',
                        onConfirm: async () => {
                          try {
                            await dropCheckConstraint({
                              source: { name: source.name, kind: source.kind },
                              table: table.table,
                              constraintName: constraint.name,
                            });
                            return true;
                          } catch {
                            return false;
                          }
                        },
                      })
                    }
                  />
                )}
                <Text>
                  <Strong>{constraint.name}</Strong> &middot; {constraint.check}
                </Text>
              </Flex>
            ))}
          </div>
        ))}

      {canAdd && (
        <Button
          type="button"
          mode="default"
          size="1"
          leftIcon={FaPlus}
          onClick={() => setIsAddDialogOpen(true)}
        >
          Add Check Constraint
        </Button>
      )}

      {canAdd && isAddDialogOpen && (
        <CheckConstraintForm
          title="Add Check Constraint"
          submitLabel="Add"
          defaultValues={emptyCheckConstraintFormValues}
          isPending={isPending}
          onSubmit={onSubmit}
          onClose={() => setIsAddDialogOpen(false)}
        />
      )}
    </div>
  );
};

export default CheckConstraints;
