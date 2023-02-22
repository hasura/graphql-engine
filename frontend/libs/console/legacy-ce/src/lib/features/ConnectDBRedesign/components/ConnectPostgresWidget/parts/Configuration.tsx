import { InputField } from '../../../../../new-components/Form';
import { ConnectionInfo } from './ConnectionInfo';

export const Configuration = ({
  name,
  hideOptions,
}: {
  name: string;
  hideOptions: string[];
}) => {
  return (
    <div className="my-2">
      <ConnectionInfo
        name={`${name}.connectionInfo`}
        hideOptions={hideOptions}
      />
      <div className="mt-sm">
        <InputField
          name={`${name}.extensionSchema`}
          label="Extension Schema"
          placeholder="public"
          tooltip="Name of the schema where the graphql-engine will install database extensions (default: `public`). Specified schema should be present in the search path of the database."
        />
      </div>
    </div>
  );
};
