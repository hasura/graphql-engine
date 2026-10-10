import { useState } from 'react';
import { Flex, Strong } from '@radix-ui/themes';
import {
  Badge,
  Button,
  Collapsible,
  IconButton,
  SkeletonList,
  SqlCodeBlock,
  Text,
  useDestructiveConfirm,
} from '@hasura/shared/ui';
import { FaPlus, FaTrash } from 'react-icons/fa';
import {
  getDatabaseMethods,
  TableTrigger,
  useCreateTrigger,
  useDropTrigger,
  useTableTriggers,
  useTriggerFunctions,
} from '@hasura/metadata/data-source';
import { ModifyTableProps } from '../types';
import {
  getEmptyTriggerFormValues,
  TriggerForm,
  TriggerFormValues,
} from './TriggerForm';

/** Postgres triggers that Hasura generates for its event triggers. Removing
 *  one by hand silently breaks the event trigger, so they're read-only here. */
const isHasuraEventTrigger = (trigger: TableTrigger) =>
  trigger.name.startsWith('notify_hasura_') ||
  Boolean(trigger.definition?.includes('hdb_catalog."notify_hasura_'));

/** The full `CREATE TRIGGER` statement, or — when the driver can't introspect
 *  it — a summary built from the timing, events and action. */
const getTriggerDefinition = (trigger: TableTrigger) =>
  trigger.createStatement ??
  [
    `-- ${[trigger.timing, trigger.events].filter(Boolean).join(' ')}`,
    trigger.definition,
  ]
    .filter(Boolean)
    .join('\n');

/**
 * DB trigger editor: list / add / remove, each gated on the real driver method
 * (`getTableTriggers` / `createTrigger` / `dropTrigger`). PG family only.
 */
export const Triggers = ({ source, table }: ModifyTableProps) => {
  const dbMethods = getDatabaseMethods(source.kind);
  const canAdd = Boolean(dbMethods.modify?.createTrigger);
  const canDrop = Boolean(dbMethods.modify?.dropTrigger);

  const destructiveConfirm = useDestructiveConfirm();
  const [isAddDialogOpen, setIsAddDialogOpen] = useState(false);

  const { mutateAsync: dropTrigger } = useDropTrigger();
  const {
    mutate: createTrigger,
    isPending,
    error,
  } = useCreateTrigger({ onSuccess: () => setIsAddDialogOpen(false) });

  const { data: triggers = [], isLoading } = useTableTriggers({
    source,
    table: table.table,
  });
  const { data: functions = [] } = useTriggerFunctions(
    { source },
    {
      enabled:
        isAddDialogOpen && Boolean(dbMethods.introspection.getTriggerFunctions),
    },
  );

  const tableSchema = (table.table as { schema?: string }).schema ?? 'public';

  const onSubmit = (values: TriggerFormValues) => {
    const isNewFunction = values.functionMode === 'new';
    const fn = isNewFunction
      ? { schema: values.newFunctionSchema, name: values.newFunctionName }
      : values.existingFunction;
    if (!fn) return;

    createTrigger({
      source: { name: source.name, kind: source.kind },
      table: table.table,
      triggerName: values.name,
      timing: values.timing,
      events: values.events,
      forEach: values.forEach,
      condition: values.condition || undefined,
      function: fn,
      newFunctionBody: isNewFunction ? values.newFunctionBody : undefined,
    });
  };

  if (isLoading) return <SkeletonList count={2} />;

  return (
    <>
      {triggers.length === 0 ? (
        <Text as="p">No triggers found.</Text>
      ) : (
        triggers.map((trigger) => {
          const isEventTrigger = isHasuraEventTrigger(trigger);
          return (
            <div key={trigger.name} className="mb-1">
              <Flex align="start" gap="2">
                {canDrop && !isEventTrigger && (
                  <IconButton
                    type="button"
                    mode="destructive"
                    size="1"
                    variant="outline"
                    icon={FaTrash}
                    title={`Remove trigger ${trigger.name}`}
                    onClick={() =>
                      destructiveConfirm({
                        resourceName: trigger.name,
                        resourceType: 'Trigger',
                        onConfirm: async () => {
                          try {
                            await dropTrigger({
                              source: { name: source.name, kind: source.kind },
                              table: table.table,
                              triggerName: trigger.name,
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
                <div className="min-w-0 flex-1">
                  <Collapsible
                    disableContentStyles
                    triggerChildren={
                      <Flex align="center" gap="2">
                        <Text>
                          <Strong>{trigger.name}</Strong>
                          {trigger.timing || trigger.events
                            ? ` · ${[trigger.timing, trigger.events]
                                .filter(Boolean)
                                .join(' ')}`
                            : ''}
                        </Text>
                        {isEventTrigger && (
                          <Badge
                            color="gray"
                            title="Generated by a Hasura event trigger; manage it from the Events tab"
                          >
                            event trigger
                          </Badge>
                        )}
                      </Flex>
                    }
                  >
                    <SqlCodeBlock
                      language={source.kind}
                      text={getTriggerDefinition(trigger)}
                    />
                  </Collapsible>
                </div>
              </Flex>
            </div>
          );
        })
      )}

      {canAdd && (
        <div className="mt-2">
          <Button
            type="button"
            size="sm"
            mode="default"
            leftIcon={FaPlus}
            onClick={() => setIsAddDialogOpen(true)}
          >
            Add Trigger
          </Button>
        </div>
      )}

      {canAdd && isAddDialogOpen && (
        <TriggerForm
          defaultValues={getEmptyTriggerFormValues(tableSchema)}
          functions={functions}
          isPending={isPending}
          error={error}
          onSubmit={onSubmit}
          onClose={() => setIsAddDialogOpen(false)}
        />
      )}
    </>
  );
};

export default Triggers;
