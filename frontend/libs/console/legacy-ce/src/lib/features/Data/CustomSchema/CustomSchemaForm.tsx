import clsx from 'clsx';
import { useFormContext } from 'react-hook-form';
import { FaPlay } from 'react-icons/fa';
import { Button } from '../../../new-components/Button';
import { Dialog } from '../../../new-components/Dialog';
import { FieldWrapper, InputField, Radio } from '../../../new-components/Form';
import { Tabs } from '../../../new-components/Tabs';
import { GraphQLSchemaInput } from './GraphQLSchemaInput/GraphQLSchemaInput';
import { useCustomSchemaForm } from './hooks/useCustomSchemaForm';
import { JsonSchemaInput } from './JsonSchemaInput/JsonSchemaInput';
import { CustomSchemaFormVals } from './types';

export type CustomSchemaFormProps = {
  jsonSchema?: string;
  graphqlSchema?: string;
  onSubmit: (data: CustomSchemaFormVals) => void;
  onClose: () => void;
  callToAction?: string;
  callToActionLoadingText?: string;
  callToDeny?: string;
};

export const CustomSchemaForm: React.VFC<CustomSchemaFormProps> = props => {
  const { Form, handleSubmit } = useCustomSchemaForm(props);

  return (
    <Form onSubmit={handleSubmit}>
      <div>
        <div className="pl-md pr-md pb-sm">
          <div className={clsx('flex text-muted flex-col')}>
            <div>
              Customize tracked objects and their GraphQL API representations.
            </div>
          </div>
          <Tabs
            items={[
              {
                value: 'schema-setup',
                label: 'Schema Setup',
                content: (
                  <div className="pt-8">
                    <CustomSchemaFields {...props} />
                  </div>
                ),
              },
            ]}
          />
        </div>
        <Dialog.Footer
          callToAction={props.callToAction}
          callToActionLoadingText={props.callToActionLoadingText}
          callToDeny={props.callToDeny}
          className="sticky w-full bottom-0 left-0 z-10"
        />
      </div>
    </Form>
  );
};

function CustomSchemaFields(props: CustomSchemaFormProps) {
  const { watch } = useFormContext<CustomSchemaFormVals>();
  const schemaType = watch('schemaType');

  return (
    <>
      <div>
        <Radio
          name="schemaType"
          label="Schema Type"
          options={[
            {
              value: 'graphql',
              label: 'GraphQL',
            },
            {
              value: 'json',
              label: 'MongoDB JSON',
            },
          ]}
          orientation="horizontal"
        />
      </div>
      {schemaType === 'graphql' ? (
        <div>
          <GraphQLSchemaField />
        </div>
      ) : (
        <>
          <div>
            <InputField
              name="schemaSamplingSize"
              label="Schema Sampling Size"
              description="How many records should be sampled from Collections for inferring the Collection’s schema?"
              placeholder="1000"
              type="number"
              rightButton={
                <Button icon={<FaPlay className="w-3 h-3 text-base" />}>
                  Sample Schema
                </Button>
              }
            />
          </div>
          <div>
            <JsonSchemaField />
          </div>
        </>
      )}
    </>
  );
}

function GraphQLSchemaField() {
  const { setValue } = useFormContext<CustomSchemaFormVals>();
  const name = 'graphqlSchema';
  const { watch } = useFormContext<CustomSchemaFormVals>();
  const value = watch(name);

  return (
    <FieldWrapper
      label="Schema"
      description="How many records should be sampled from Collections for inferring the Collection’s schema?"
    >
      <GraphQLSchemaInput
        value={value}
        onChange={newValue => setValue(name, newValue)}
      />
    </FieldWrapper>
  );
}

function JsonSchemaField() {
  const { setValue } = useFormContext<CustomSchemaFormVals>();
  const name = 'jsonSchema';
  const { watch } = useFormContext<CustomSchemaFormVals>();
  const value = watch(name);

  return (
    <FieldWrapper
      label="Schema"
      description="How many records should be sampled from Collections for inferring the Collection’s schema?"
    >
      <JsonSchemaInput
        value={value}
        onChange={newValue => setValue(name, newValue)}
      />
    </FieldWrapper>
  );
}
