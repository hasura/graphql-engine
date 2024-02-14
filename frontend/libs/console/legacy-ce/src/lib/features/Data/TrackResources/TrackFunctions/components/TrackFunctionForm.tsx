import { z } from 'zod';
import { Dialog } from '../../../../../new-components/Dialog';
import { Select, useConsoleForm } from '../../../../../new-components/Form';
import { AllowedFunctionTypes } from './UntrackedFunctions';
import { useUntrackedFunctions } from '../hooks/useUntrackedFunctions';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import { adaptFunctionName } from '../utils';
import {
  MetadataSelectors,
  useMetadata,
} from '../../../../hasura-metadata-api';

import { DisplayToastErrorMessage } from '../../../components/DisplayErrorMessage';
import { hasuraToast } from '../../../../../new-components/Toasts';
import { useTrackFunction } from '../../../hooks/useTrackFunction';
import { getQualifiedTable } from '../../../ManageTable/utils';

const validationSchema = z.object({
  qualifiedFunction: z.any(),
  table: z.string().min(1, { message: 'The return type is mandatory' }),
  type: z.union([
    z.literal('mutation'),
    z.literal('query'),
    z.literal('root_field'),
  ]),
});

export type TrackFunctionFormSchema = z.infer<typeof validationSchema>;

const allowedFunctionTypes: AllowedFunctionTypes[] = [
  'mutation',
  'query',
  'root_field',
];

export type TrackFunctionFormProps = {
  dataSourceName: string;
  onSuccess: () => void;
  onClose: () => void;
  defaultValues?: TrackFunctionFormSchema;
};

export const TrackFunctionForm = ({
  dataSourceName,
  onSuccess,
  onClose,
  defaultValues,
}: TrackFunctionFormProps) => {
  const { data: untrackedFunctions = [] } =
    useUntrackedFunctions(dataSourceName);

  const { trackFunction, isLoading: isTrackingInProgress } = useTrackFunction({
    dataSourceName: dataSourceName,
    onSuccess: () => {
      onClose();
      hasuraToast({
        type: 'success',
        title: 'Success',
        message: `Tracked object successfully`,
      });
    },
    onError: err => {
      hasuraToast({
        type: 'error',
        title: err.name,
        children: <DisplayToastErrorMessage message={err.message} />,
      });
    },
  });

  const {
    Form,
    methods: {
      handleSubmit,
      formState: { errors },
    },
  } = useConsoleForm({
    schema: validationSchema,
    options: {
      defaultValues: defaultValues,
    },
  });

  const { data: tableOptions = [] } = useMetadata(m =>
    MetadataSelectors.findSource(dataSourceName)(m)?.tables.map(t => ({
      label: getQualifiedTable(t.table).join(' / '),
      value: JSON.stringify(t.table),
    }))
  );

  const onHandleSubmit = (data: TrackFunctionFormSchema) => {
    trackFunction({
      functionsToBeTracked: [
        {
          function: JSON.parse(data.qualifiedFunction),
          configuration: {
            ...(data.type !== 'root_field' ? { exposed_as: data.type } : {}),
            response: {
              type: 'table',
              table: JSON.parse(data.table),
            },
          },
        },
      ],
    });
  };

  console.log(errors);

  return (
    <Dialog
      hasBackdrop
      title="Track Function"
      onClose={onClose}
      footer={
        <Dialog.Footer
          onSubmit={() => {
            handleSubmit(onHandleSubmit)();
          }}
          isLoading={isTrackingInProgress}
          onClose={onClose}
          callToDeny="Cancel"
          callToAction="Track Function"
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
          <Select
            label="Track Function"
            name="qualifiedFunction"
            placeholder="Select a function"
            options={untrackedFunctions.map(f => ({
              value: JSON.stringify(f.qualifiedFunction),
              label: adaptFunctionName(f.qualifiedFunction).join(' / '),
            }))}
            disabled
          />

          <Select
            label="Tracked as"
            name="type"
            placeholder="Select type"
            options={allowedFunctionTypes.map(type => ({
              value: type,
              label: type,
            }))}
            disabled
          />

          <Select
            label="Select a return type"
            placeholder="Return type must be one of the tables tracked"
            name="table"
            options={tableOptions}
            disabled={!tableOptions.length}
          />
          {!tableOptions.length && (
            <IndicatorCard headline="You do not have any tables tracked for this database">
              Tables that are tracked in Hasura can be used as the return type
              for your function.
            </IndicatorCard>
          )}
        </Form>
      </div>
    </Dialog>
  );
};
