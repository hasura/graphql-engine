import { Collapse } from '../../../../../../new-components/deprecated';
import React from 'react';
import styles from '../../DataSources.module.scss';

export const FormContainer: React.FC = ({ children }) => (
  <div className="w-full mb-md">
    <div className="cursor-pointer w-full flex-initial align-middle">
      <Collapse title="Connection Settings">
        <Collapse.Content>
          <div className={styles.connection_settings_form}>{children}</div>
        </Collapse.Content>
      </Collapse>
    </div>
  </div>
);
