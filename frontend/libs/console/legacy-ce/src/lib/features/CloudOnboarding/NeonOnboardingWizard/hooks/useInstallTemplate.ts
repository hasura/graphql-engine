import { useCallback } from 'react';
import { useInstallMigration } from './useInstallMigration';
import { getMetadataUrl, getMigrationUrl } from '../../constants';
import { useInstallMetadata } from './useInstallMetadata';

/**
 * used to install the template, which is a combination of applying migrations,
 * and then applying metadata to the specified data source.
 * @return A memoised function which can be called imperatively to initiate the install.
 */
export function useInstallTemplate(
  dataSourceName: string,
  templateBaseUrl: string,
  onSuccessCb: () => void,
  onErrorCb: (errorMsg?: string) => void
) {
  // fetch the function to apply metadata
  const { updateMetadata } = useInstallMetadata(
    dataSourceName,
    getMetadataUrl(templateBaseUrl),
    onSuccessCb,
    onErrorCb
  );

  // fetch the function to apply migration
  const { performMigration } = useInstallMigration(
    dataSourceName,
    getMigrationUrl(templateBaseUrl),
    // install metadata only if migrations has been applied successfully
    () => {
      if (updateMetadata) {
        updateMetadata();
      }
    },
    onErrorCb
  );

  const install = useCallback(() => {
    // only do a template install if both `performMigration` and `updateMetadata` functions are defined.
    // otherwise `install` will just return an empty function. In that case, error callbacks will have info on what went wrong.
    if (performMigration && updateMetadata) {
      // only `performMigration` is called while invoking `install`,
      // which in turn calls the `updateMetadata` if migration application was successful.
      performMigration();
    }
  }, [performMigration, updateMetadata]);

  return { install };
}
