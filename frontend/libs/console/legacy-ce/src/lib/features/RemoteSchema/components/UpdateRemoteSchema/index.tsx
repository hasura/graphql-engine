import { useNavigate } from 'react-router';
import { appPrefix, pageTitle } from '../../constants';
import { RemoteSchemaForm } from '../Form/Form';
import type { RemoteSchema } from '@hasura/shared/types';
import { useCurrentRemoteSchemaContext } from '../../context';
import { createRemoteSchemaFormValues } from '../Form/Form/utils';
import { useDocumentTitle } from '@hasura/shared/hooks';
import {
  useInconsistentMetadata,
  useRemoveRemoteSchema,
  useUpdateRemoteSchema,
} from '@hasura/metadata/api';

import { getConfirmation } from '@hasura/shared/utils';
import { Tabs } from '../Tabs';
import { InconsistentBadge } from '../InconsistentBadge';

const UpdateRemoteSchema = () => {
  const navigate = useNavigate();
  const { currentRemoteSchema } = useCurrentRemoteSchemaContext();
  const { data: inconsistentMetadata } = useInconsistentMetadata();

  const { mutate: remove, isPending: isRemoving } = useRemoveRemoteSchema();
  const { mutate: update, isPending: isUpdating } = useUpdateRemoteSchema();

  useDocumentTitle(`Edit ${pageTitle} - ${currentRemoteSchema.name} | Hasura`);

  const onSubmit = (args: RemoteSchema) => {
    return update(args);
  };

  const onDelete: React.MouseEventHandler<HTMLButtonElement> = (e) => {
    e.preventDefault();

    const remoteSchemaName = currentRemoteSchema.name;
    const confirmMessage = `This will remove the remote GraphQL schema "${remoteSchemaName}" from your GraphQL schema`;
    const isOk = getConfirmation(confirmMessage, true, remoteSchemaName);
    if (!isOk) {
      return;
    }

    return remove(currentRemoteSchema.name, () => {
      navigate(appPrefix);
    });
  };

  const defaultValues = createRemoteSchemaFormValues(currentRemoteSchema);

  const breadCrumbs = [
    {
      title: 'Remote schemas',
      url: appPrefix,
    },
    {
      title: 'Manage',
      url: appPrefix + '/' + 'manage',
    },
    {
      title: currentRemoteSchema.name,
      url:
        appPrefix +
        '/' +
        'manage' +
        '/' +
        encodeURIComponent(currentRemoteSchema.name) +
        '/' +
        'details',
    },
    {
      title: 'modify',
      url: '',
    },
  ];

  const inconsistencyDetails = inconsistentMetadata?.inconsistent_objects?.find(
    (inconObj) =>
      'type' in inconObj &&
      inconObj.type === 'remote_schema' &&
      inconObj.definition.name === currentRemoteSchema.name,
  );

  return (
    <div>
      <Tabs
        currentTab="modify"
        heading={currentRemoteSchema.name}
        breadCrumbs={breadCrumbs}
        baseUrl={`${appPrefix}/manage/${encodeURIComponent(
          currentRemoteSchema.name ?? '',
        )}`}
      />

      {inconsistencyDetails && (
        <InconsistentBadge inconsistencyDetails={inconsistencyDetails} />
      )}

      <RemoteSchemaForm
        saving={isUpdating}
        deleting={isRemoving}
        defaultValues={defaultValues}
        existingCustomization={currentRemoteSchema.definition?.customization}
        onDelete={onDelete}
        onSubmit={onSubmit}
        edit
      />
    </div>
  );
};

export default UpdateRemoteSchema;
