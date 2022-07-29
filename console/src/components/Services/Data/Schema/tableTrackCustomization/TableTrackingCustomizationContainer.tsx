import {
  getTableBrowseRoute,
  getTableModifyRoute,
} from '@/components/Common/utils/routesUtils';
import { Driver } from '@/dataSources';
import {
  allowedMetadataTypes,
  useMetadataMigration,
} from '@/features/MetadataAPI';
import { useFireNotification } from '@/new-components/Notifications';
import { ReduxState } from '@/types';
import React from 'react';
import { useDispatch } from 'react-redux';
import { AnyAction } from 'redux';
import { ThunkDispatch } from 'redux-thunk';
import { REQUEST_SUCCESS, updateSchemaInfo } from '../../DataActions';
import { setSidebarLoading } from '../../DataSubSidebar';
import _push from '../../push';
import { FormValues } from './TableTrackingCustomizationForm';
import {
  TableTrackingCustomizationModal,
  TableTrackingCustomizationModalProps,
} from './TableTrackingCustomizationModal';
import { getQualifiedTable, getTrackTableType } from './utils';

type TableTrackingCustomizationModalContainerProps = Omit<
  TableTrackingCustomizationModalProps,
  'onSubmit'
> & { dataSource: string; driver: Driver; schema: string };

export const TableTrackingCustomizationModalContainer: React.FC<TableTrackingCustomizationModalContainerProps> = ({
  onClose,
  tableName,
  schema,
  dataSource,
  driver,
}) => {
  const { fireNotification } = useFireNotification();
  const dispatch: ThunkDispatch<ReduxState, unknown, AnyAction> = useDispatch();

  const mutation = useMetadataMigration({
    onSuccess: () => {
      dispatch({ type: REQUEST_SUCCESS });
      dispatch(updateSchemaInfo()).then(() => {
        const nextRoute =
          driver !== 'bigquery'
            ? getTableModifyRoute(schema, dataSource, tableName, true)
            : getTableBrowseRoute(schema, dataSource, tableName, true);
        dispatch(_push(nextRoute));
        dispatch(setSidebarLoading(false));
        fireNotification({
          title: 'Success!',
          message: 'Existing table/view added',
          type: 'success',
        });
        if (onClose) onClose();
      });
    },
    onError: (error: Error) => {
      fireNotification({
        title: 'Error',
        message: error?.message ?? 'Error while adding table/view',
        type: 'error',
      });
    },
  });

  const onCustomizationFormSubmit = (values: FormValues) => {
    const requestBody = {
      type: getTrackTableType(driver) as allowedMetadataTypes,
      args: {
        source: dataSource,
        table: getQualifiedTable({
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
          },
        },
      },
    };
    mutation.mutate({
      query: requestBody,
    });
  };

  return (
    <TableTrackingCustomizationModal
      tableName={tableName}
      onSubmit={onCustomizationFormSubmit}
      onClose={onClose}
      isLoading={mutation.isLoading}
    />
  );
};
