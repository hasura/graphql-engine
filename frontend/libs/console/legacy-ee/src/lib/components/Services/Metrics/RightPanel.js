import React from 'react';

import Helmet from 'react-helmet';

import { urlToPageHeaderMap } from './utils';

import AllowListHeader from './AllowLists/AllowListHeader';

import { strippedCurrUrl } from './helpers';
import { ApiLimitsHeader } from './ApiLimits/ApiLimitsHeader';
import OperationsHeader from './Operations/OperationsHeader';
import styles from './Metrics.module.scss';

const RightPanel = props => {
  const { location, dispatch, projectConfig, metadata, refetchMetadata } =
    props;
  const { pathname } = location;

  const strippedUrl = strippedCurrUrl(pathname);

  const getHeaderName = () => {
    const headerName = urlToPageHeaderMap[strippedUrl];
    if (headerName === 'Overview') {
      return 'Overview';
    }
    if (headerName === 'Allow Lists') {
      return <AllowListHeader title={headerName} />;
    }
    if (headerName === 'API Limits') {
      return <ApiLimitsHeader />;
    }
    if (headerName === 'Operations') {
      return (
        <OperationsHeader
          dispatch={dispatch}
          metadata={metadata}
          refetchMetadata={refetchMetadata}
          projectConfig={projectConfig}
        />
      );
    }
    return headerName;
  };
  const pageName = getHeaderName();

  const headerTitle =
    urlToPageHeaderMap[strippedUrl] === 'Overview'
      ? 'Overview'
      : urlToPageHeaderMap[strippedUrl];

  return (
    <div className={styles.RightPanelWrapper}>
      <div className={styles.headerWrapper}>
        <Helmet title={`${headerTitle} - Metrics | Hasura`} />
        <div className={styles.header}>{pageName || ''}</div>
        {/*
        <div className={styles.projectName}>myproject-prod</div>
        */}
        {/*
        <div className={styles.deadBtn}>
          <button>DEAD</button>
        </div>
        */}
        {/*
        <div className={styles.activeBtn}>
          <button>ALIVE</button>
        </div>
        */}
      </div>
      <div className={styles.usageWrapper}>{props.children}</div>
    </div>
  );
};

export default RightPanel;
