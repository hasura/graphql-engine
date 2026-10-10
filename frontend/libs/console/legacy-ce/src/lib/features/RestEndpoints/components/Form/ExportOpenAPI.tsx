import { useMutation } from '@tanstack/react-query';
import { Button, showErrorNotification } from '@hasura/shared/ui';
import { FaFileExport } from 'react-icons/fa';
import { hasuraToast } from '@hasura/shared/ui';
import { Analytics } from '@hasura/shared/analytics';
import { downloadObjectAsJsonFile } from '@hasura/shared/utils';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';

export const ExportOpenApiButton = () => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  const { mutate, isPending } = useMutation({
    mutationFn: () => {
      return fetchJson(endpoints.exportOpenApi);
    },
    onSuccess: (data) => {
      downloadObjectAsJsonFile('OpenAPISpec.json', data);
      hasuraToast({
        title: 'OpenApi spec exported Successfully!',
        type: 'success',
      });
    },
    onError: (error) => {
      showErrorNotification({
        title: 'Unable to Export!',
        error,
      });
    },
  });

  return (
    <Analytics name="export-open-api-spec-btn" passHtmlAttributesToChildren>
      <Button
        mode="default"
        leftIcon={FaFileExport}
        onClick={() => mutate()}
        loading={isPending}
      >
        Export OpenAPI Spec
      </Button>
    </Analytics>
  );
};
