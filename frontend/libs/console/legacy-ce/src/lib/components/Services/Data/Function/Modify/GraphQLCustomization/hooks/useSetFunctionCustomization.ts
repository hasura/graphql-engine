import { getDriverPrefix } from '../../../../../../../features/DataSource';
import {
  QualifiedFunction,
  SupportedDrivers,
  Table,
} from '../../../../../../../features/hasura-metadata-types';
import {
  allowedMetadataTypes,
  useMetadataMigration,
} from '../../../../../../../features/MetadataAPI';
import { useAppDispatch } from '../../../../../../../storeHooks';
import { updateSchemaInfo } from '../../../../DataActions';

type Configuration = {
  custom_root_fields?: {
    function_aggregate?: string;
    function?: string;
  };
  custom_name?: string;
  response?: {
    table: Table;
    type: string;
  };
};

type OnSetFunctionCustomizationArgs = {
  driver: SupportedDrivers;
  dataSourceName: string;
  // the plain function name is used by native databases, the qualified function by GDC data sources
  functionName: string | QualifiedFunction;
  configuration: Configuration;
};

type Props = {
  onSuccess?: () => void;
  onError?: (error: Error) => void;
};

export const useSetFunctionCustomization = ({ onSuccess, onError }: Props) => {
  const dispatch = useAppDispatch();

  const mutation = useMetadataMigration({
    onSuccess: () => {
      dispatch(updateSchemaInfo()).then(() => {
        if (onSuccess) {
          onSuccess();
        }
      });
    },
    onError: (error: Error) => {
      if (onError) {
        onError(error);
      }
    },
  });

  const onSetFunctionCustomization = ({
    driver,
    dataSourceName,
    functionName,
    configuration,
  }: OnSetFunctionCustomizationArgs) => {
    const requestBody = {
      type: `${getDriverPrefix(
        driver
      )}_set_function_customization` as allowedMetadataTypes,
      args: {
        source: dataSourceName,
        function: functionName,
        configuration,
      },
    };
    return mutation.mutate({
      query: requestBody,
    });
  };

  return { ...mutation, onSetFunctionCustomization };
};
