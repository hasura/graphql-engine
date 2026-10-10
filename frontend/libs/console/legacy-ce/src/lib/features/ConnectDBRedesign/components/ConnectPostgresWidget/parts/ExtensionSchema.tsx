import { InputField } from '@hasura/shared/ui';

export const ExtensionSchema = ({ name }: { name: string }) => {
  return (
    <InputField
      name={name}
      label="Extension Schema"
      tooltip="Name of the schema where the graphql-engine will install database extensions (default: `public`). Specified schema should be present in the search path of the database."
      fieldProps={{
        placeholder: 'public',
      }}
    />
  );
};
