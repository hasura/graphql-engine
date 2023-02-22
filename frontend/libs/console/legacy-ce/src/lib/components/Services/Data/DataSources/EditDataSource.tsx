import React from 'react';
import Helmet from 'react-helmet';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';
import BreadCrumb from '../../../Common/Layout/BreadCrumb/BreadCrumb';
import { RightContainer } from '../../../Common/Layout/RightContainer';
import styles from './DataSources.module.scss';

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
      <Analytics name="EditDataSource" {...REDACT_EVERYTHING}>
        <div className={styles.add_pad_left_mid}>
          <BreadCrumb breadCrumbs={breadCrumbs} />
        </div>
      </Analytics>
      {children}
    </RightContainer>
  );
};

export default EditDataSource;
