import { Dialog } from '../../../new-components/Dialog';
import { LogicalModel, MetadataTableConfig } from '../../hasura-metadata-types';
import {
  ErrorComponentTemplate,
  Radio,
  Select,
  useConsoleForm,
} from '../../../new-components/Form';
import { z } from 'zod';
import AceEditor from '../../../components/Common/AceEditor/BaseEditor';
import { ReactElement, useState } from 'react';
import { Collapsible } from '../../../new-components/Collapsible';
import { CardedTable } from '../../../new-components/CardedTable';
import { CollectionGraphQLCustomizations } from './GraphQLCustomizations';
import { Analytics } from '../../Analytics';
import { LogicalModelsBadge } from './components/LogicalModelsBadge';
import { SuccessLogicalModelsInferBadge } from './components/SuccessLogicalModelsInferBadge';
import { FaExclamationCircle } from 'react-icons/fa';
import { inferLogicalModels } from './utils/inferLogicalModel';
import { JSONValidationSchemaBadge } from './components/JSONValidationSchemaBadge';
import { hasuraToast } from '../../../new-components/Toasts';

const options = [
  { value: 'sample-documents', label: 'Sample documents', checked: true },
  { value: 'logical-models', label: 'Existing Logical Models' },
  { value: 'json-validation-schema', label: 'JSON Validation Schema' },
];

const twLabelClassNames = 'block pt-1 text-gray-600 font-semibold';
const Label = ({ children }: { children: string | ReactElement }) => (
  <div className={twLabelClassNames}>{children}</div>
);

const validationSchema = z
  .object({
    logicalModelForm: z.union([
      z.literal('sample-documents'),
      z.literal('logical-models'),
      z.literal('json-validation-schema'),
    ]),
    logicalModel: z.string().optional(),
    sampleDocument: z.string().optional(),
    custom_name: z.string().optional(),
    custom_root_fields: z
      .object({
        select: z.string().optional(),
        select_by_pk: z.string().optional(),
        select_aggregate: z.string().optional(),
        select_stream: z.string().optional(),
        insert: z.string().optional(),
        insert_one: z.string().optional(),
        update: z.string().optional(),
        update_by_pk: z.string().optional(),
        delete: z.string().optional(),
        delete_by_pk: z.string().optional(),
        update_many: z.string().optional(),
      })
      .optional(),
  })
  .refine(
    data => {
      if (
        data.logicalModelForm === 'sample-documents' &&
        !data.sampleDocument
      ) {
        return false;
      }
      return true;
    },
    {
      message: 'The Sample Document is required',
      path: ['sampleDocument'],
    }
  )
  .refine(
    data => {
      if (data.logicalModelForm === 'logical-models' && !data.logicalModel) {
        return false;
      }
      return true;
    },
    {
      message: 'The Logical Model is required',
      path: ['logicalModel'],
    }
  );

export type Schema = z.infer<typeof validationSchema>;

export type MongoTrackCollectionModalProps = {
  dataSourceName: string;
  collectionName: string;
  onSubmit: (data: Schema, inferredLogicalModels: LogicalModel[]) => void;
  onClose: () => void;
  isLoading: boolean;
  isVisible: boolean;
  currentConfiguration?: MetadataTableConfig;
  logicalModels: LogicalModel[];
  collectionConfiguration?: MetadataTableConfig;
};

export const MongoTrackCollectionModal = ({
  collectionName,
  dataSourceName,
  collectionConfiguration = {},
  onClose,
  isVisible,
  logicalModels,
  onSubmit,
  isLoading,
}: MongoTrackCollectionModalProps) => {
  const [view, setView] = useState<'prepare' | 'validate'>('prepare');

  const [inferredLogicalModels, setInferredLogicalModels] = useState<
    LogicalModel[]
  >([]);

  const {
    methods: {
      watch,
      setValue,
      formState: { errors },
    },
    Form,
  } = useConsoleForm({
    schema: validationSchema,
    options: {
      defaultValues: {
        logicalModelForm: 'sample-documents',
        logicalModel: logicalModels[0]?.name || '',
        ...collectionConfiguration,
      },
    },
  });

  const prepareCustomCollectionName = watch('custom_name');

  const [isAutoLogicalModelsBadgeVisible, setAutoLogicalModelsBadgeVisible] =
    useState(false);

  const selection = watch('logicalModelForm');
  const sampleDocument = watch('sampleDocument');

  const {
    methods: { watch: validateWatch, setValue: validateSetValue },
    Form: ValidateForm,
  } = useConsoleForm({
    schema: validationSchema,
  });

  const customCollectionName = validateWatch('custom_name');
  const allValidateFormFields = validateWatch();

  const syncCustomizationFieldsToPrepare = () => {
    const fields = allValidateFormFields;
    const customRootFields = fields?.custom_root_fields || {};
    setValue('custom_name', fields.custom_name || '');
    Object.keys(customRootFields).forEach(key => {
      setValue(`custom_root_fields.${key}`, customRootFields[key] || '');
    });
  };

  const syncCustomizationFieldsToValidate = (
    data: z.infer<typeof validationSchema>
  ) => {
    validateSetValue('logicalModel', data.logicalModel);
    validateSetValue('logicalModelForm', data.logicalModelForm);
    validateSetValue('sampleDocument', data.sampleDocument);

    if (data.custom_name) {
      validateSetValue('custom_name', data.custom_name);
    }

    if (data?.custom_root_fields) {
      Object.keys(data.custom_root_fields).forEach(key => {
        const k = key as keyof Schema['custom_root_fields'];
        validateSetValue(
          `custom_root_fields.${key}`,
          data?.custom_root_fields?.[k] || ''
        );
      });
    }
  };

  const onPrepareSubmit = (data: z.infer<typeof validationSchema>) => {
    try {
      const validation = validationSchema.safeParse(data);

      if (!validation.success) {
        return;
      }

      if (data.logicalModelForm === 'json-validation-schema') {
        onFinalSubmit(data);
        return;
      }

      if (data.logicalModelForm === 'sample-documents' && data.sampleDocument) {
        // infer the logical models from sampleDocument
        const logicalModels = inferLogicalModels(
          data.custom_name || collectionName,
          data.sampleDocument
        );
        setInferredLogicalModels(
          logicalModels.map(model => ({ ...model, dataSourceName }))
        );

        setAutoLogicalModelsBadgeVisible(true);
        // NOTE: it's important to sync the Customization fields before changing the view
        syncCustomizationFieldsToValidate(data);
        setView('validate');
      }

      if (data.logicalModelForm === 'logical-models' && data.logicalModel) {
        // set logical model
        // go to the next view
        setAutoLogicalModelsBadgeVisible(false);
        const logicalModel = logicalModels.find(
          model => model.name === data.logicalModel
        );
        if (logicalModel) {
          setInferredLogicalModels([logicalModel]);
          // NOTE: it's important to sync the Customization fields before changing the view
          syncCustomizationFieldsToValidate(data);
          setView('validate');
        }
      }
    } catch (err: any) {
      hasuraToast({
        type: 'error',
        title: 'An error occurred',
        message: err?.message || '',
      });
    }
  };

  const onFinalSubmit = (data: z.infer<typeof validationSchema>) => {
    onSubmit(data, inferredLogicalModels);
  };

  return isVisible ? (
    <Dialog
      hasBackdrop
      title="Track Collection"
      description={
        <>
          Generate schema and track for <b>{collectionName}</b> collection.
        </>
      }
      onClose={onClose}
    >
      <>
        {view === 'prepare' && (
          <Form onSubmit={onPrepareSubmit}>
            <div>
              <div className="px-4 pb-sm">
                <LogicalModelsBadge />

                <div className="flex items-center">
                  <Radio
                    name="logicalModelForm"
                    label="Logical model from"
                    options={options}
                    orientation="horizontal"
                  />
                </div>
                {selection === 'sample-documents' && (
                  <div>
                    <Label>Sample Document</Label>
                    <div className="text-muted pb-xs">
                      Auto-generate Logical Models based on a sample document
                    </div>
                    {errors.sampleDocument && (
                      <ErrorComponentTemplate
                        label={
                          <>
                            <FaExclamationCircle className="fill-current h-4 w-4 mr-xs shrink-0" />
                            {errors.sampleDocument.message}
                          </>
                        }
                        ariaLabel={errors.sampleDocument.message ?? ''}
                        role="alert"
                      />
                    )}
                    <div className="mt-0.5">
                      <AceEditor
                        name="json-editor"
                        mode="json"
                        placeholder="Your sample document here"
                        value={sampleDocument}
                        onChange={value => setValue('sampleDocument', value)}
                        fontSize="12px"
                        height="200px"
                        width="100%"
                        showPrintMargin={false}
                        setOptions={{ useWorker: false }}
                      />
                    </div>
                  </div>
                )}
                {selection === 'logical-models' && (
                  <Analytics name="logicalModel" htmlAttributesToRedact="value">
                    <Select
                      name="logicalModel"
                      options={
                        logicalModels?.map(model => ({
                          label: model.name,
                          value: model.name,
                        })) ?? []
                      }
                    />
                  </Analytics>
                )}
                {selection === 'json-validation-schema' && (
                  <JSONValidationSchemaBadge />
                )}
                <div className="mt-sm">
                  <Collapsible
                    triggerChildren={
                      <span className="flex items-baseline">
                        <Label>Advanced Configuration</Label>
                        <span className="text-muted ml-1">(Optional)</span>
                      </span>
                    }
                    children={
                      <CollectionGraphQLCustomizations
                        collectionName={collectionName}
                        customCollectionName={
                          prepareCustomCollectionName ||
                          customCollectionName ||
                          collectionName
                        }
                      />
                    }
                  />
                </div>
              </div>
              <Dialog.Footer
                callToAction={
                  selection === 'json-validation-schema'
                    ? 'Track Collection'
                    : 'Validate'
                }
                isLoading={isLoading}
                callToActionLoadingText="Generating..."
                callToDeny="Cancel"
                onClose={onClose}
                className="sticky w-full bottom-0 left-0"
                /* leftContent={<LearnMoreLink href="" />} */
              />
            </div>
          </Form>
        )}
        {view === 'validate' && (
          <ValidateForm onSubmit={onFinalSubmit}>
            <div>
              <div className="px-4 pb-sm">
                {isAutoLogicalModelsBadgeVisible && (
                  <SuccessLogicalModelsInferBadge />
                )}
                <Label>Logical Models</Label>
                {inferredLogicalModels.map((logicalModel, index) => {
                  return (
                    <Collapsible
                      triggerChildren={<Label>{logicalModel.name}</Label>}
                      key={index}
                      children={
                        <div>
                          <div className="mt-xs">
                            <Label>Data Source name</Label>
                          </div>
                          <div className="px-1.5 py-2.5 bg-slate-100 rounded">
                            {dataSourceName}
                          </div>

                          <div className="mt-xs">
                            <Label>Logical Model Name</Label>
                          </div>
                          <div className="px-1.5 py-2.5 bg-slate-100 rounded">
                            {logicalModel.name}
                          </div>

                          <div className="mt-xs">
                            <Label>Fields</Label>
                          </div>
                          <CardedTable
                            columns={['name', 'type', 'nullable', 'array']}
                            data={logicalModel.fields.map(field => {
                              const type =
                                'scalar' in field.type
                                  ? field.type.scalar
                                  : 'array' in field.type &&
                                    'scalar' in field.type.array
                                  ? field.type.array.scalar
                                  : 'Logical Model';

                              const nullable =
                                'nullable' in field.type && field.type.nullable;
                              const array = 'array' in field.type;

                              return [
                                field.name,
                                type,
                                nullable.toString(),
                                array.toString(),
                              ];
                            })}
                          />
                        </div>
                      }
                    />
                  );
                })}

                <div className="mt-sm">
                  <Collapsible
                    triggerChildren={
                      <span className="flex items-baseline">
                        <Label>Advanced Configuration</Label>
                        <span className="text-muted ml-1">(Optional)</span>
                      </span>
                    }
                    children={
                      <CollectionGraphQLCustomizations
                        collectionName={collectionName}
                        customCollectionName={
                          prepareCustomCollectionName ||
                          customCollectionName ||
                          collectionName
                        }
                      />
                    }
                  />
                </div>
              </div>
              <Dialog.Footer
                callToAction="Track Collection"
                isLoading={isLoading}
                callToActionLoadingText="Tracking..."
                callToDeny="Back"
                onClose={() => {
                  syncCustomizationFieldsToPrepare();
                  setView('prepare');
                }}
                className="sticky w-full bottom-0 left-0"
                /* leftContent={<LearnMoreLink href="" />} */
              />
            </div>
          </ValidateForm>
        )}
      </>
    </Dialog>
  ) : null;
};
