import { InputField } from '@hasura/shared/ui';
import { ConnectionInfo } from './ConnectionInfo';

export const Configuration = ({
  name,
  hideOptions,
}: {
  name: string;
  hideOptions: string[];
}) => {
  return (
    <div className="my-2 px-4">
      <ConnectionInfo
        name={`${name}.connectionInfo`}
        hideOptions={hideOptions}
      />
      <div className="mt-2">
        <InputField
          name={`${name}.extensionSchema`}
          label="Extension Schema"
          tooltip="Name of the schema where the graphql-engine will install database extensions (default: `public`). Specified schema should be present in the search path of the database."
          fieldProps={{
            placeholder: 'public',
          }}
        />
      </div>
    </div>
  );
};
