import { urlToPageHeaderMap } from './utils';
import { strippedCurrUrl } from './helpers';
import styles from './Metrics.module.scss';
import { useLocation } from 'react-router';
import { useDocumentTitle } from '@hasura/shared/hooks';

const RightPanel = ({ children }) => {
  const location = useLocation();
  const strippedUrl = strippedCurrUrl(location.pathname);

  const getHeaderName = () => {
    const headerName = urlToPageHeaderMap[strippedUrl];
    if (headerName === 'Overview') {
      return 'Overview';
    }

    return headerName;
  };
  const pageName = getHeaderName();

  const headerTitle =
    urlToPageHeaderMap[strippedUrl] === 'Overview'
      ? 'Overview'
      : urlToPageHeaderMap[strippedUrl];

  useDocumentTitle(`${headerTitle} - Metrics | Hasura`);

  return (
    <div className={styles['RightPanelWrapper']}>
      <div className={styles['headerWrapper']}>
        <div className={styles['header']}>{pageName || ''}</div>
      </div>
      <div className={styles['usageWrapper']}>{children}</div>
    </div>
  );
};

export default RightPanel;
