import React from 'react';
import Tooltip from '../../../../Common/Tooltip/Tooltip';
import { EventTrigger } from '../../types';

type ETInfoProps = {
  currentTrigger: EventTrigger;
  styles: Record<string, any>;
};

const Info = ({ currentTrigger, styles }: ETInfoProps) => (
  <div className={`${styles.container} ${styles.borderBottom}`}>
    <div className={styles.modifySection}>
      <h4 className={styles.modifySectionHeading}>
        Info{' '}
        <Tooltip message="Delete this trigger and create another one if you need to edit any of these fields" />
      </h4>
      <div className={styles.infoContent}>
        <table className="table table-striped table-bordered">
          <thead />
          <tbody>
            <tr>
              <td>Trigger name</td>
              <td>{currentTrigger.name}</td>
            </tr>
            <tr>
              <td>Table</td>
              <td>{currentTrigger.table_name}</td>
            </tr>
            <tr>
              <td>Schema</td>
              <td>{currentTrigger.schema_name}</td>
            </tr>
            <tr>
              <td>Data source</td>
              <td>{currentTrigger.source}</td>
            </tr>
          </tbody>
        </table>
      </div>
    </div>
  </div>
);

export default Info;
