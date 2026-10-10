import { useNavigate } from 'react-router';
import { appPrefix } from '../../constants';
import { RemoteSchemaForm } from '../Form/Form';
import { useAddRemoteSchema } from '@hasura/metadata/api';
import type { RemoteSchema } from '@hasura/shared/types';
import { createRemoteSchemaFormValues } from '../Form/Form/utils';
import { useDocumentTitle } from '@hasura/shared/hooks';

const CreateRemoteSchema = () => {
  useDocumentTitle('Create Remote Schema | Hasura');

  const navigate = useNavigate();
  const { mutate, isPending: isLoading } = useAddRemoteSchema();

  const onSubmit = (args: RemoteSchema) => {
    return mutate(args, (remoteSchemaName) => {
      navigate(
        `${appPrefix}/manage/${encodeURIComponent(
          remoteSchemaName ?? '',
        )}/details`,
      );
    });
  };

  const defaultValues = createRemoteSchemaFormValues();

  return (
    <RemoteSchemaForm
      saving={isLoading}
      onSubmit={onSubmit}
      defaultValues={defaultValues}
    />
  );
};

export default CreateRemoteSchema;
