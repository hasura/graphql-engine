import React, { FormEvent } from 'react';
import { Button } from '@/new-components/Button';
import ConnectDatabaseForm, { ConnectDatabaseFormProps } from './ConnectDBForm';
import styles from './DataSources.module.scss';

interface DataSourceFormWrapperProps extends ConnectDatabaseFormProps {
  loading: boolean;
  onSubmit: (e: FormEvent<HTMLFormElement>) => void;
}

const DataSourceFormWrapper: React.FC<DataSourceFormWrapperProps> = ({
  onSubmit,
  loading,
  isEditState,
  children,
  ...props
}) => {
  return (
    <form
      onSubmit={onSubmit}
      className={`${styles.connect_db_content} ${styles.connect_form_width}`}
    >
      <ConnectDatabaseForm isEditState={isEditState} {...props} />
      {children}
      <div className={styles.add_button_layout}>
        <Button
          size="lg"
          mode="primary"
          type="submit"
          style={{
            ...(loading && { cursor: 'progress' }),
          }}
          isLoading={loading}
          loadingText="Saving..."
          data-test="connect-database-btn"
          data-trackid="data-tab-connect-db-button"
        >
          {!isEditState ? 'Connect Database' : 'Update Connection'}
        </Button>
      </div>
    </form>
  );
};
export default DataSourceFormWrapper;
