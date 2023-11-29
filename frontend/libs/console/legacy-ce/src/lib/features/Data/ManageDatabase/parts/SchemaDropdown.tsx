import { BsDatabaseFillGear } from 'react-icons/bs';
import { FaRegTrashAlt } from 'react-icons/fa';
import { GrTableAdd } from 'react-icons/gr';
import { MdEditNote, MdOutlineCreateNewFolder } from 'react-icons/md';
import { DropDown } from '../../../../new-components/AdvancedDropDown';
import {
  useDestructiveAlert,
  useHasuraAlert,
} from '../../../../new-components/Alert';
import { Button } from '../../../../new-components/Button';
import { usePushRoute } from '../../../ConnectDBRedesign/hooks/usePushRoute';
import { MetadataSelectors, useMetadata } from '../../../hasura-metadata-api';
import {
  useCreateDatabaseSchema,
  useDeleteDatabaseSchema,
} from '../../hooks/modify';
import { addMutateAsyncTuple, handleRunSqlError } from '../../hooks/utils';

type SchemaDropdownProps = {
  schemas: string[];
  dataSourceName: string;
};

export function SchemaDropdown({
  schemas,
  dataSourceName,
}: SchemaDropdownProps) {
  const { mutateAsyncTuple: createSchema } = addMutateAsyncTuple(
    useCreateDatabaseSchema({
      dataSourceName,
    })
  );
  const { mutateAsyncTuple: deleteSchema } = addMutateAsyncTuple(
    useDeleteDatabaseSchema({
      dataSourceName,
    })
  );

  const { data: source, isSuccess } = useMetadata(
    MetadataSelectors.findSource(dataSourceName)
  );

  const push = usePushRoute();

  const { hasuraPrompt } = useHasuraAlert();

  const { destructiveConfirm } = useDestructiveAlert();

  const handleDropSchema = (name: string) => {
    destructiveConfirm({
      resourceName: name,
      resourceType: 'Schema',
      destroyTerm: 'delete',

      onConfirm: async () => {
        const [err] = await deleteSchema({ schemaName: name });

        if (err) {
          handleRunSqlError(err);
          return false;
        }
        return true;
      },
    });
  };

  const handleCreateSchema = () => {
    hasuraPrompt({
      message: 'Type a name for your new schema:',
      title: 'Create Schema',
      sanitizeGraphQL: true,
      confirmText: 'Create',
      onCloseAsync: async result => {
        if (!result.confirmed) return;

        const [err] = await createSchema({
          schemaName: result.promptValue,
        });

        if (err) {
          handleRunSqlError(err);
          return { withSuccess: false };
        }

        return { withSuccess: true, successText: 'Schema Created' };
      },
    });
  };

  const handlePermissionsSummary = (schema: string) => {
    push(`data/${dataSourceName}/schema/${schema}/permissions`);
  };

  const handleCreateTable = (schema: string) => {
    push(`data/${dataSourceName}/schema/${schema}/table/add`);
  };

  return (
    <DropDown.Root
      trigger={
        <Button size="sm" className="mr-2" icon={<BsDatabaseFillGear />} />
      }
    >
      {isSuccess && source?.kind && (
        <>
          <DropDown.Label>Connection</DropDown.Label>
          <DropDown.BasicItem
            link
            onClick={() =>
              push(
                `data/v2/manage/database/edit?driver=${source.kind}&database=${dataSourceName}`
              )
            }
          >
            <div className="flex gap-3 items-center">
              <MdEditNote className="text-subtitle" />
              Edit Connection
            </div>
          </DropDown.BasicItem>
        </>
      )}
      <DropDown.Label>Schemas</DropDown.Label>
      <DropDown.BasicItem link onClick={handleCreateSchema}>
        <div className="flex gap-3 items-center">
          <MdOutlineCreateNewFolder className="text-subtitle" /> Create
          Schema...
        </div>
      </DropDown.BasicItem>
      <DropDown.SubMenu
        label={
          <div className="flex gap-3 text-red-500 items-center">
            <FaRegTrashAlt />
            Delete Schema...
          </div>
        }
      >
        <DropDown.Label>Choose Schema To Delete...</DropDown.Label>
        {schemas.map(name => (
          <DropDown.BasicItem
            onClick={() => handleDropSchema(name)}
            dangerous
            key={name}
          >
            {name}
          </DropDown.BasicItem>
        ))}
      </DropDown.SubMenu>
      <DropDown.SubMenu label="Permissions Summary">
        <DropDown.Label>Choose Schema To View...</DropDown.Label>
        {schemas.map(name => (
          <DropDown.BasicItem
            onClick={() => handlePermissionsSummary(name)}
            key={name}
          >
            {name}
          </DropDown.BasicItem>
        ))}
      </DropDown.SubMenu>
      <DropDown.Label>Tables</DropDown.Label>
      <DropDown.SubMenu
        label={
          <div className="flex gap-3 items-center">
            <GrTableAdd />
            Create Table..
          </div>
        }
      >
        <DropDown.Label>Choose Schema To Add Table...</DropDown.Label>
        {schemas.map(name => (
          <DropDown.BasicItem
            onClick={() => handleCreateTable(name)}
            key={name}
          >
            {name}
          </DropDown.BasicItem>
        ))}
      </DropDown.SubMenu>
    </DropDown.Root>
  );
}
