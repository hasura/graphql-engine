import { InputField } from '../../../../../new-components/Form';

export const ExtensionSchema = ({ name }: { name: string }) => {
  return (
    <InputField
      name={name}
      label="Extension Schema"
      placeholder="public"
      tooltip="Name of the schema where the graphql-engine will install database extensions (default: `public`). Specified schema should be present in the search path of the database."
    />
  );
};
