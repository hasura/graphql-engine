import { z } from 'zod';
import {
  Dialog,
  InputField,
  DialogFooter,
  useConsoleForm,
  hasuraToast,
  Collapsible,
  showErrorNotification,
  Text,
  RadioGroupField,
} from '@hasura/shared/ui';
import { MetadataFunction, Source } from '@hasura/shared/types';
import { useSetFunctionConfiguration } from '../../hooks/useSetFunctionConfiguration';
import { cleanEmpty } from '../../../ConnectDBRedesign/components/ConnectPostgresWidget/utils/helpers';
import { functionDisplayName } from '@hasura/metadata/helpers';

export type ModifyFunctionConfigurationProps = {
  currentFunction: MetadataFunction;
  source: Source;
  isVolatile?: boolean;
  onClose: () => void;
  onSuccess: () => void;
};

const exposeAsEnums = ['query', 'mutation'] as const;

const validationSchema = z.object({
  custom_name: z.string().optional(),
  custom_root_fields: z
    .object({
      function: z.string().optional(),
      function_aggregate: z.string().optional(),
    })
    .optional(),
  session_argument: z.string().optional(),
  exposed_as: z.enum(exposeAsEnums).default('query'),
});

export type Schema = z.infer<typeof validationSchema>;

export const ModifyFunctionConfiguration = ({
  source,
  currentFunction,
  onSuccess,
  onClose,
  isVolatile,
}: ModifyFunctionConfigurationProps) => {
  const { setFunctionConfiguration, isPending } = useSetFunctionConfiguration({
    dataSourceName: source.name,
  });

  const {
    Form,
    methods: { handleSubmit, watch },
  } = useConsoleForm({
    schema: validationSchema,
    options: {
      defaultValues: {
        custom_name: currentFunction?.configuration?.custom_name ?? '',
        custom_root_fields: currentFunction?.configuration?.custom_root_fields,
        exposed_as: currentFunction.configuration?.exposed_as ?? 'query',
        session_argument: currentFunction.configuration?.session_argument,
      },
    },
  });

  const onHandleSubmit = (data: Schema) => {
    setFunctionConfiguration({
      qualifiedFunction: currentFunction.function,
      // the comment is edited separately; don't drop it when saving the rest
      configuration: {
        ...(currentFunction.configuration?.comment
          ? { comment: currentFunction.configuration.comment }
          : {}),
        ...cleanEmpty(data),
      },
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: 'Success',
          message: `Updated successfully`,
        });
        onSuccess();
      },
      onError: (err) => {
        showErrorNotification({
          title: 'Updating function failed',
          error: err,
        });
      },
    });
  };

  const customName = watch('custom_name');
  const functionName = functionDisplayName({
    qualifiedFunction: currentFunction.function,
    separator: '_',
  });

  const exposedAsOptions = exposeAsEnums.map((exposedAs) => ({
    label: exposedAs,
    value: exposedAs,
    disabled: exposedAs === 'mutation' && !isVolatile,
  }));

  const disabled = isPending;

  return (
    <Dialog
      title="Edit Function Configuration"
      onClose={onClose}
      footer={
        <DialogFooter
          onSubmit={() => {
            handleSubmit(onHandleSubmit)();
          }}
          isLoading={isPending}
          onClose={onClose}
          callToDeny="Cancel"
          callToAction="Save Configuration"
          onSubmitAnalyticsName="actions-tab-generate-types-submit"
          onCancelAnalyticsName="actions-tab-generate-types-cancel"
        />
      }
    >
      <Form onSubmit={() => {}}>
        <InputField
          name="custom_name"
          label="Custom Name"
          fieldProps={{ placeholder: functionName, clearable: true, disabled }}
          tooltip="The GraphQL nodes for the function will be generated according to the custom name"
          learnMoreLink="https://hasura.io/docs/latest/graphql/core/schema/custom-functions.html#custom-function-root-fields"
        />

        <InputField
          name="session_argument"
          label="Session argument"
          tooltip="Function argument which accepts session info JSON"
          learnMoreLink="http://hasura.io/docs/2.0/schema/postgres/custom-functions/#accessing-hasura-session-variables-in-custom-functions"
          fieldProps={{
            placeholder: 'hasura_session',
            disabled,
          }}
        />
        <div className="mb-4">
          <RadioGroupField
            label="Exposed as"
            orientation="horizontal"
            tooltip="In which part of the schema should we expose this function?"
            name="exposed_as"
            options={exposedAsOptions}
            noErrorPlaceholder
            disabled={disabled}
          />
        </div>
        <Collapsible
          triggerChildren={<Text weight="medium">Custom Root Fields</Text>}
          defaultOpen={Boolean(
            currentFunction?.configuration?.custom_name ||
            currentFunction?.configuration?.custom_root_fields,
          )}
        >
          <div className="mb-4">
            <InputField
              name="custom_root_fields.function"
              label="Function"
              noErrorPlaceholder
              fieldProps={{
                placeholder: customName || functionName,
                clearable: true,
                disabled,
              }}
              tooltip="Customize the <function-name> root field"
            />
          </div>
          <InputField
            name="custom_root_fields.function_aggregate"
            label="Function Aggregate"
            noErrorPlaceholder
            fieldProps={{
              placeholder: `${customName || functionName}_aggregate`,
              clearable: true,
              disabled,
            }}
            tooltip="Customize the <function-name>_aggregate root field"
          />
        </Collapsible>
      </Form>
    </Dialog>
  );
};
