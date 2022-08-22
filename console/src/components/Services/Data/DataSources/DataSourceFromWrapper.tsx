import React, { FormEvent } from 'react';
import { Button } from '../../../Common';
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
          size="large"
          color="yellow"
          type="submit"
          style={{
            ...(loading && { cursor: 'progress' }),
          }}
          disabled={loading}
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
