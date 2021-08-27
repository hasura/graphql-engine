import React, { useMemo } from 'react';
import { Link } from 'react-router';

import YouTube from 'react-youtube';

import globals from '../../../Globals';
import { hasSources, isMetadataEmpty } from '../../../metadata/utils';
import { setOnboardingCompletedInDB } from '../../../telemetry/Actions';
import { Dispatch, ReduxState } from '../../../types';
import { getLSItem, LS_KEYS, setLSItem } from '../../../utils/localStorage';
import hasuraDarkIcon from './hasura_icon_dark.svg';
import styles from './Onboarding.scss';

type PopupLinkProps = {
  title: string;
  index: number;
  link?: {
    pro: string;
    cloud: string;
    oss: string;
  };
  internalLink?: string;
  icon?: string;
  videoId?: string;
};

const PopupLink = ({
  link,
  index,
  videoId,
  title,
  internalLink,
  icon,
}: PopupLinkProps) => {
  if (videoId) {
    return (
      <li className={`${styles.popup_item} ${styles.video}`}>
        <>
          <div className={styles.link_container}>
            <span className={styles.link_num}>{index}</span>
            {title}
          </div>
          <YouTube
            videoId={videoId}
            opts={{ width: '100%', height: '240px' }}
          />
        </>
      </li>
    );
  }
  let url = link?.oss;
  if (globals.serverVersion.includes('pro')) {
    url = link?.pro;
  } else if (globals.serverVersion.includes('cloud')) {
    url = link?.cloud;
  }
  return (
    <li className={styles.popup_item}>
      {internalLink ? (
        <Link
          to={internalLink}
          className={`${styles.link_container} ${styles.link}`}
        >
          <div className={styles.helper_circle}>
            <i className={`fa ${icon}`} />
          </div>
          {title}
        </Link>
      ) : (
        <a
          href={url}
          target="_blank"
          rel="noopener noreferrer"
          className={`${styles.link_container} ${styles.link}`}
        >
          <span className={styles.link_num}>{index}</span>
          {title}
          <div className={styles.spacer} />
          <i
            className={`fa fa-external-link ${styles.link_icon} ${styles.muted}`}
          />
        </a>
      )}
    </li>
  );
};

const connectDatabaseHelper = {
  title: ' Connect Your First Database',
  internalLink: '/data/manage',
  icon: 'fa-database',
};

const onboardingList = [
  {
    title: 'Read the Getting Started Docs',
    link: {
      pro:
        'https://hasura.io/docs/latest/graphql/core/getting-started/first-graphql-query.html?pg=pro&plcmt=onboarding-checklist#create-a-table',
      oss:
        'https://hasura.io/docs/latest/graphql/core/getting-started/first-graphql-query.html?pg=oss-console&plcmt=onboarding#create-a-table',
      cloud:
        'https://hasura.io/docs/latest/graphql/core/getting-started/first-graphql-query.html?pg=cloud&plcmt=onboarding-checklist#create-a-table',
    },
  },
  { title: 'Watch Our Getting Started Video', videoId: 'ZGKQ0U18USU' },
  {
    title: 'Bookmark Our Course',
    link: {
      pro:
        'https://hasura.io/learn/graphql/hasura-advanced/introduction/?pg=pro&plcmt=onboarding-checklist',
      oss:
        'https://hasura.io/learn/graphql/hasura/introduction/?pg=oss-console&plcmt=onboarding-checklist',
      cloud:
        'https://hasura.io/learn/graphql/hasura/introduction/?pg=cloud&plcmt=onboarding-checklist',
    },
  },
];

interface OnboardingProps {
  dispatch: Dispatch;
  console_opts: ReduxState['telemetry']['console_opts'];
  metadata: ReduxState['metadata']['metadataObject'];
}

const Onboarding: React.FC<OnboardingProps> = ({
  dispatch,
  console_opts,
  metadata,
}) => {
  const [visible, setVisible] = React.useState(true);

  const shouldShowOnbaording = useMemo(() => {
    const shown = console_opts && console_opts.onboardingShown;
    if (shown) {
      return false;
    }
    if (!metadata) {
      return true;
    }
    return isMetadataEmpty(metadata) && !shown;
  }, [metadata, console_opts]);

  React.useEffect(() => {
    const show = getLSItem(LS_KEYS.showConsoleOnboarding) || 'true';
    setVisible(show === 'true');
  }, []);

  const togglePopup = () => {
    setVisible(pre => {
      setLSItem(LS_KEYS.showConsoleOnboarding, (!pre).toString());
      return !pre;
    });
  };

  const markCompleted = () => {
    dispatch(setOnboardingCompletedInDB);
  };

  if (!shouldShowOnbaording) {
    return null;
  }

  return (
    <>
      {!visible && (
        <div className={styles.hi_icon} onClick={togglePopup}>
          <span aria-label="Wave" role="img">
            {' '}
            👋{' '}
          </span>
        </div>
      )}
      {visible && (
        <div className={styles.onboarding_popup} data-test="onboarding-popup">
          <div
            className={styles.popup_header}
            data-test="onboarding-popup-header"
          >
            <img src={hasuraDarkIcon} alt="Hasura Logo" />
            <strong>Hi there, let&apos;s get started with Hasura!</strong>
          </div>
          <div className={styles.popup_body}>
            <ul>
              {metadata && !hasSources(metadata) ? (
                <PopupLink {...connectDatabaseHelper} index={0} />
              ) : null}
              {onboardingList.map((item, i) => (
                <PopupLink {...item} key={i} index={i + 1} />
              ))}
            </ul>
          </div>
          <div className={styles.popup_buttons}>
            <button
              onClick={togglePopup}
              className={styles.button}
              data-test="btn-hide-for-now"
            >
              Hide for now
            </button>
            <button
              onClick={markCompleted}
              className={`${styles.button} ${styles.muted}`}
              data-test="btn-ob-dont-show-again"
            >
              Don&apos;t show me again
            </button>
          </div>
        </div>
      )}
    </>
  );
};

export default Onboarding;
