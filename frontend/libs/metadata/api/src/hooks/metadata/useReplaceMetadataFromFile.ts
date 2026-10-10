import { useReplaceMetadata } from './useReplaceMetadata';
import { isConsoleError } from '@hasura/shared/utils';
import { hasuraToast } from '@hasura/shared/ui';

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
        hasuraToast({
          type: 'error',
          title: 'Error parsing metadata file',
          message: e.message,
        });
      }

      if (onError) onError();

      return;
    }

    return replaceMetadata(parsedFileContent, onSuccess, onError);
  };
};
