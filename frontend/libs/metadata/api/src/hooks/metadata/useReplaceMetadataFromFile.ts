import { useReplaceMetadata } from './useReplaceMetadata';
import { isConsoleError } from '@hasura/shared/utils';
import { showErrorNotification } from '@hasura/shared/ui';

export const useReplaceMetadataFromFile = () => {
  const replaceMetadata = useReplaceMetadata();

  return (
    fileContent: string,
    onSuccess?: () => void,
    onError?: () => void,
  ) => {
    let parsedFileContent: any;

    try {
      parsedFileContent = JSON.parse(fileContent);
    } catch (e) {
      if (isConsoleError(e)) {
        showErrorNotification({
          title: 'Error parsing metadata file',
          error: e,
        });
      }

      if (onError) onError();

      return;
    }

    return replaceMetadata(parsedFileContent, onSuccess, onError);
  };
};
