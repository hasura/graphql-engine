import { dataRoutes } from '@hasura/shared/utils';
import { useMetadataMigration } from '@hasura/metadata/api';
import {
  CustomFieldNamesModal,
  CustomFieldNamesModalProps,
} from './CustomFieldNamesModal';

import { CustomFieldNamesFormVals } from './types';
import {
  getQualifiedTableForCustomFieldNames,
  getTrackTableType,
} from './utils';
import { hasuraToast } from '@hasura/shared/ui';
import { useNavigate } from 'react-router';
import { SupportedDriver } from '@hasura/shared/types';

type LegacyWrapperProps = Omit<
  CustomFieldNamesModalProps,
  'onSubmit' | 'source' | 'isLoading'
> & {
  dataSource: string;
  driver: SupportedDriver;
  schema: string;
};

export const LegacyWrapper: React.FC<LegacyWrapperProps> = (props) => {
  const { tableName, schema, dataSource, driver, onClose } = props;
  const navigate = useNavigate();

  const mutation = useMetadataMigration({
    onSuccess: () => {
      hasuraToast({
        title: 'Success!',
        message: 'Existing table/view added',
        type: 'success',
      });
      if (onClose) onClose();
      const nextRoute =
        driver !== 'bigquery'
          ? dataRoutes.getTableModifyRoute(schema, dataSource, tableName, true)
          : dataRoutes.getTableBrowseRoute(schema, dataSource, tableName, true);
      navigate(nextRoute);
      // dispatch(setSidebarLoading(false));
    },
    onError: (error: Error) => {
      hasuraToast({
        title: 'Error',
        message: error?.message ?? 'Error while adding table/view',
        type: 'error',
      });
    },
  });

  const onCustomizationFormSubmit = (values: CustomFieldNamesFormVals) => {
    const requestBody = {
      type: getTrackTableType(driver),
      args: {
        source: dataSource,
        table: getQualifiedTableForCustomFieldNames({
          driver,
          tableName,
          schema,
        }),
        configuration: {
          custom_name: values.custom_name,
          custom_root_fields: {
            select: values.select,
            select_by_pk: values.select_by_pk,
            select_aggregate: values.select_aggregate,
            select_stream: values.select_stream,
            insert: values.insert,
            insert_one: values.insert_one,
            update: values.update,
            update_by_pk: values.update_by_pk,
            delete: values.delete,
            delete_by_pk: values.delete_by_pk,
            update_many: values.update_many,
          },
        },
      },
    };
    mutation.mutate({
      query: requestBody,
    });
  };

  return (
    <CustomFieldNamesModal
      {...props}
      onSubmit={onCustomizationFormSubmit}
      isLoading={mutation.isPending}
      source={dataSource}
    />
  );
};
