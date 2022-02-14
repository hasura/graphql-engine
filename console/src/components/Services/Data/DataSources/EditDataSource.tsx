import React from 'react';
import Helmet from 'react-helmet';
import BreadCrumb from '../../../Common/Layout/BreadCrumb/BreadCrumb';
import { RightContainer } from '../../../Common/Layout/RightContainer';
import styles from './DataSources.scss';

const appPrefix = '/data';
const breadCrumbs = [
  {
    title: 'Data',
    url: appPrefix,
  },
  {
    title: 'Data Manager',
    url: `${appPrefix}/manage`,
  },
  {
    title: 'Edit Data Source',
    url: '',
  },
];

const EditDataSource: React.FC = ({ children }) => {
  return (
    <RightContainer>
      <Helmet title="Edit Data Source - Hasura" />
      <div className={styles.add_pad_left_mid}>
        <BreadCrumb breadCrumbs={breadCrumbs} />
      </div>
      {children}
    </RightContainer>
  );
};

export default EditDataSource;
