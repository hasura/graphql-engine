import { z } from 'zod';
import { Dialog } from '../../../../new-components/Dialog';
import {
  InputField,
  Select,
  useConsoleForm,
} from '../../../../new-components/Form';
import { QualifiedFunction } from '../../../hasura-metadata-types';
import { useEffect } from 'react';
import {
  MetadataSelectors,
  areTablesEqual,
  useMetadata,
} from '../../../hasura-metadata-api';
import { getQualifiedTable } from '../../ManageTable/utils';
import { useSetFunctionConfiguration } from '../../hooks/useSetFunctionConfiguration';
import { hasuraToast } from '../../../../new-components/Toasts';
import { DisplayToastErrorMessage } from '../../components/DisplayErrorMessage';
import { cleanEmpty } from '../../../ConnectDBRedesign/components/ConnectPostgresWidget/utils/helpers';
import { Collapsible } from '../../../../new-components/Collapsible';
import { adaptFunctionName } from '../../TrackResources/TrackFunctions';

export type ModifyFunctionConfigurationProps = {
  qualifiedFunction: QualifiedFunction;
  dataSourceName: string;
  onClose: () => void;
  onSuccess: () => void;
};

const validationSchema = z.object({
  custom_name: z.string().optional(),
  custom_root_fields: z
    .object({
      function: z.string().optional(),
      function_aggregate: z.string().optional(),
    })
    .optional(),
  response: z
    .object({
      type: z.literal('table'),
      table: z.string().min(1, { message: 'The return type is mandatory' }),
    })
    .optional(),
});

export type Schema = z.infer<typeof validationSchema>;

export const ModifyFunctionConfiguration = (
  props: ModifyFunctionConfigurationProps
) => {
  const { setFunctionConfiguration, isLoading } = useSetFunctionConfiguration({
    dataSourceName: props.dataSourceName,
  });

  const { data: tableOptions = [] } = useMetadata(m =>
    MetadataSelectors.findSource(props.dataSourceName)(m)?.tables.map(t => ({
      label: getQualifiedTable(t.table).join(' / '),
      value: JSON.stringify(t.table),
    }))
  );

  const { data: metadataFunction, isFetched } = useMetadata(m =>
    MetadataSelectors.findSource(props.dataSourceName)(m)?.functions?.find(f =>
      areTablesEqual(f.function, props.qualifiedFunction)
    )
  );

  const {
    Form,
    methods: { handleSubmit, reset, watch },
  } = useConsoleForm({
    schema: validationSchema,
  });

  useEffect(() => {
    if (isFetched && metadataFunction) {
      reset({
        ...metadataFunction.configuration,
        response: {
          type: metadataFunction.configuration?.response?.type ?? 'table',
          table: JSON.stringify(
            metadataFunction.configuration?.response?.table
          ),
        },
      });
    }
  }, [isFetched, metadataFunction, reset]);

  const onHandleSubmit = (data: Schema) => {
    setFunctionConfiguration({
      qualifiedFunction: props.qualifiedFunction,
      configuration: cleanEmpty({
        ...data,
        response: {
          ...data.response,
          table: data.response?.table ? JSON.parse(data.response?.table) : {},
        },
      }),
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: 'Success',
          message: `Updated successfully`,
        });
        props.onSuccess();
      },
      onError: err => {
        hasuraToast({
          type: 'error',
          title: err.name,
          children: <DisplayToastErrorMessage message={err.message} />,
        });
      },
    });
  };

  const customName = watch('custom_name');

  return (
    <Dialog
      hasBackdrop
      title="Edit Function Configuration"
      onClose={props.onClose}
      footer={
        <Dialog.Footer
          onSubmit={() => {
            handleSubmit(onHandleSubmit)();
          }}
          isLoading={isLoading}
          onClose={props.onClose}
          callToDeny="Cancel"
          callToAction="Save Configuration"
          onSubmitAnalyticsName="actions-tab-generate-types-submit"
          onCancelAnalyticsName="actions-tab-generate-types-cancel"
        />
      }
    >
      <div className="p-4">
        <Form
          onSubmit={data => {
            console.log('>>>', data);
          }}
        >
          <InputField
            name="custom_name"
            label="Custom Name"
            placeholder={adaptFunctionName(props.qualifiedFunction).join('_')}
            clearButton
            tooltip="The GraphQL nodes for the function will be generated according to the custom name"
          />

          <Collapsible
            triggerChildren={
              <div className="font-semibold text-muted">Custom Root Fields</div>
            }
          >
            <InputField
              name="custom_root_fields.function"
              label="Function"
              placeholder={
                customName?.length
                  ? customName
                  : adaptFunctionName(props.qualifiedFunction).join('_')
              }
              tooltip="Customize the <function-name> root field"
              clearButton
            />
            <InputField
              name="custom_root_fields.function_aggregate"
              label="Function Aggregate"
              placeholder={`${
                customName?.length
                  ? customName
                  : adaptFunctionName(props.qualifiedFunction).join('_')
              }_aggregate`}
              tooltip="Customize the <function-name>_aggregate root field"
              clearButton
            />
          </Collapsible>

          <Collapsible
            triggerChildren={
              <div className="font-semibold text-muted">Response Settings</div>
            }
            defaultOpen
          >
            <div className="hidden">
              <InputField name="response.type" label="type" />
            </div>

            <Select
              label="Select a return type"
              placeholder="Return type must be one of the tables tracked"
              name="response.table"
              options={tableOptions}
            />
          </Collapsible>
        </Form>
      </div>
    </Dialog>
  );
};
