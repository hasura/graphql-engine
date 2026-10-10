import {
  GraphQLSanitizedInputField,
  ListMap,
  SelectField,
} from '@hasura/shared/ui';

export const TrackNativeQueryRelationshipForm = ({
  name,
  fromNativeQuery,
  nativeQueryOptions,
  fromFieldOptions,
  toFieldOptions,
}: {
  fromNativeQuery: string;
  nativeQueryOptions: string[];
  name?: string;
  fromFieldOptions: string[];
  toFieldOptions: string[];
}) => {
  const allowedNativeQueryOptions = nativeQueryOptions
    .filter((nq) => nq !== fromNativeQuery)
    .map((nq) => ({ value: nq, label: nq }));

  return (
    <div>
      <GraphQLSanitizedInputField
        hideTips
        label="Relationship Name"
        name={'name'}
        dataTestId="relationship_name"
        fieldProps={{ placeholder: 'Name your native query relationship' }}
      />
      <SelectField
        name={'toNativeQuery'}
        options={allowedNativeQueryOptions}
        label="Target Native Query"
        placeholder="Select target native query"
      />

      <SelectField
        name={'type'}
        options={[
          { value: 'object', label: 'Object' },
          { value: 'array', label: 'Array' },
        ]}
        placeholder="Select a relationship type..."
        label="Relationship Type"
      />

      <ListMap
        name={'columnMapping'}
        from={{
          options: fromFieldOptions,
          label: 'Source Field',
          placeholder: 'Pick source field',
        }}
        to={{
          type: 'array',
          options: toFieldOptions,
          label: 'Target Field',
          placeholder: 'Pick target field',
        }}
      />
    </div>
  );
};
