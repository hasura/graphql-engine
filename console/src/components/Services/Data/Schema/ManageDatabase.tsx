import React from 'react';
import Helmet from 'react-helmet';
import { Connect } from 'react-redux';

import Button from '../../../Common/Button/Button';
import styles from '../../../Common/Common.scss';
import { ReduxState } from '../../../../types';
import { Schema } from '../../../../dataSources/types';

type DatabaseDetails = {
  name: string;
  type: 'postgres' | 'mysql';
  url: string;
};

type ManageDatabaseProps = {
  schemaList: Schema[]; // this is just a dummy prop
};

type DatabaseListItemProps = {
  btnAction: () => void;
  btnTitle: string;
  dbDetails: DatabaseDetails;
};

const dummyData: DatabaseListItemProps[] = [
  {
    btnAction: () => {},
    btnTitle: 'Manage',
    dbDetails: { name: 'myDb', type: 'postgres', url: 'something' },
  },
  {
    btnAction: () => {},
    btnTitle: 'Remove',
    dbDetails: { name: 'dbdbdbdb', type: 'mysql', url: 'someLink' },
  },
];

const DatabaseListItem: React.FC<DatabaseListItemProps> = ({
  btnAction,
  btnTitle = 'Remove',
  dbDetails,
}) => (
  <div className={styles.db_list_item}>
    <Button size="xs" color="white" onClick={btnAction}>
      {btnTitle}
    </Button>
    <div className={styles.db_list_content}>
      <b>
        {dbDetails.name} ({dbDetails.type})
      </b>{' '}
      - {dbDetails.url}
    </div>
  </div>
);

const ManageDatabase: React.FC<ManageDatabaseProps> = ({ schemaList }) => {
  const currentSchemaOptions = schemaList.map(schema => (
    <option>{schema.schema_name}</option>
  ));
  const dataList = dummyData.map(data => <DatabaseListItem {...data} />);

  return (
    <div
      className={`container-fluid ${styles.padd_left_remove} ${styles.padd_top} ${styles.manage_dbs_page}`}
    >
      <div className={styles.padd_left}>
        <Helmet title="Manage - Data | Hasura" />
        <div className={styles.display_flex}>
          <h2 className={`${styles.headerText} ${styles.display_inline}`}>
            Manage Databases
          </h2>
          <Button color="yellow" size="md" className={styles.add_mar_left}>
            Add Database
          </Button>
        </div>
        <div className={styles.manage_db_content}>
          <hr />
          <h3 className={`${styles.heading_text} ${styles.remove_pad_bottom}`}>
            Databases
          </h3>
          <div className={styles.data_list_container}>{dataList}</div>
          <hr />
        </div>
        <div className={`${styles.manage_db_content}`}>
          <h3
            className={`${styles.heading_text} ${styles.remove_pad_bottom} ${styles.add_mar_bottom}`}
          >
            Schemas
          </h3>
          Current Schemas:
          <select className={styles.add_mar_left}>
            {currentSchemaOptions}
          </select>
          <br />
          <div
            className={`${styles.add_mar_top} ${styles.display_inline} ${styles.display_flex}`}
          >
            <Button color="white" size="xs">
              Create
            </Button>
            <Button color="white" size="xs" className={styles.add_mar_left}>
              Delete
            </Button>
          </div>
        </div>
        <hr />
      </div>
    </div>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    schemaList: state.tables.schemaList,
  };
};

const manageConnector = (connect: Connect) =>
  connect(mapStateToProps)(ManageDatabase);

export default manageConnector;
