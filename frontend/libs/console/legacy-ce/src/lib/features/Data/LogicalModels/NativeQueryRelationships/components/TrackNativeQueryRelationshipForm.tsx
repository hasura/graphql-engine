import {
  GraphQLSanitizedInputField,
  Select,
} from '../../../../../new-components/Form';
import { ListMap } from '../../../../../new-components/ListMap';

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
    .filter(nq => nq !== fromNativeQuery)
    .map(nq => ({ value: nq, label: nq }));

  return (
    <div>
      <GraphQLSanitizedInputField
        hideTips
        label="Relationship Name"
        placeholder="Name your native query relationship"
        name={'name'}
        dataTestId="relationship_name"
      />
      <Select
        name={'toNativeQuery'}
        options={allowedNativeQueryOptions}
        label="Target Native Query"
        placeholder="Select target native query"
      />

      <Select
        name={'type'}
        options={[
          { value: 'object', label: 'Object' },
          { value: 'array', label: 'Array' },
        ]}
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
