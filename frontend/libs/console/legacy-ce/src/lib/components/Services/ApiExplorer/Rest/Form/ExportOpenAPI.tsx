import { useQuery } from 'react-query';
import { Button } from '../../../../../new-components/Button';
import { FaFileExport } from 'react-icons/fa';
import { useHttpClient } from '../../../../../features/Network';
import { Axios, AxiosError } from 'axios';
import { hasuraToast } from '../../../../../new-components/Toasts';
import { Analytics } from '../../../../../features/Analytics';
import endpoints from '../../../../../Endpoints';
import { downloadObjectAsJsonFile } from '../../../../Common/utils/export.utils';

// Function to remove requestBody from GET operations in OpenAPI spec
export const removeRequestBodyFromGetOperations = (openApiSpec: any) => {
  const modifiedSpec = JSON.parse(JSON.stringify(openApiSpec));

  // Check if paths exist in the spec
  if (modifiedSpec.paths) {
    // Iterate through all paths
    Object.entries(modifiedSpec.paths).forEach(
      ([_path, pathItem]: [string, any]) => {
        // Check if there's a GET operation with requestBody
        if (pathItem.get && pathItem.get.requestBody) {
          // Remove the requestBody from GET operations
          delete pathItem.get.requestBody;
        }
      }
    );
  }

  return modifiedSpec;
};

// Export for testing
export const __testExports = {
  removeRequestBodyFromGetOperations,
};

const fetchData = async (httpClient: Axios) => {
  try {
    const response = await httpClient.get(endpoints?.exportOpenApi);
    return response.data;
  } catch (error: unknown) {
    const axiosError = error as AxiosError<{ error: string }>;
    if (axiosError.response) {
      // Server responded with a status other than 200 range
      throw new Error(axiosError.response.data.error || 'Something went wrong');
    } else {
      // Something happened while setting up the request and triggered an Error
      throw new Error('Something went wrong');
    }
  }
};

export const ExportOpenApiButton = () => {
  const httpClient = useHttpClient();
  const { isLoading, refetch, isRefetching } = useQuery(
    'exportOpenApi',
    () => fetchData(httpClient),
    {
      enabled: false,
      retry: 0,
      onSuccess: data => {
        // Process the OpenAPI spec to remove requestBody from GET operations
        const processedData = removeRequestBodyFromGetOperations(data);

        // Download the modified OpenAPI spec
        downloadObjectAsJsonFile('OpenAPISpec.json', processedData);
        hasuraToast({
          title: 'OpenApiSpec Exported Successfully!',
          type: 'success',
        });
      },
      onError: (error: Error) => {
        hasuraToast({
          title: 'Unable to Export!',
          type: 'error',
          message: error.message,
        });
      },
    }
  );

  return (
    <Analytics name="export-open-api-spec-btn" passHtmlAttributesToChildren>
      <Button
        icon={<FaFileExport />}
        className="ml-2"
        onClick={() => refetch()}
        isLoading={isLoading || isRefetching}
      >
        Export OpenAPI Spec
      </Button>
    </Analytics>
  );
};
