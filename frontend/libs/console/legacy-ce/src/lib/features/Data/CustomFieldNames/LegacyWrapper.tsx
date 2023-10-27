import {
  getTableBrowseRoute,
  getTableModifyRoute,
} from '../../../components/Common/utils/routesUtils';
import { Driver } from '../../../dataSources';
import { allowedMetadataTypes, useMetadataMigration } from '../../MetadataAPI';
import { useFireNotification } from '../../../new-components/Notifications';
import { ReduxState } from '../../../types';
import React from 'react';
import { useDispatch } from 'react-redux';
import { AnyAction } from 'redux';
import { ThunkDispatch } from 'redux-thunk';
import {
  REQUEST_SUCCESS,
  updateSchemaInfo,
} from '../../../components/Services/Data/DataActions';
import { setSidebarLoading } from '../../../components/Services/Data/DataSubSidebar';
import _push from '../../../components/Services/Data/push';
import {
  CustomFieldNamesModal,
  CustomFieldNamesModalProps,
} from './CustomFieldNamesModal';

import { CustomFieldNamesFormVals } from './types';
import {
  getQualifiedTableForCustomFieldNames,
  getTrackTableType,
} from './utils';

type LegacyWrapperProps = Omit<CustomFieldNamesModalProps, 'onSubmit'> & {
  dataSource: string;
  driver: Driver;
  schema: string;
};

export const LegacyWrapper: React.FC<LegacyWrapperProps> = props => {
  const { tableName, schema, dataSource, driver, onClose } = props;
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

  const onCustomizationFormSubmit = (values: CustomFieldNamesFormVals) => {
    const requestBody = {
      type: getTrackTableType(driver) as allowedMetadataTypes,
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
      isLoading={mutation.isLoading}
      source={dataSource}
    />
  );
};
