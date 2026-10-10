import React, { useMemo } from 'react';
import { Link } from 'react-router';
import { FaExternalLinkAlt, FaDatabase } from 'react-icons/fa';
import { Button, Separator } from '@hasura/shared/ui';
import YouTube from 'react-youtube';
import { getLSItem, setLSItem, isCloudConsole } from '@hasura/shared/utils';
import hasuraDarkIcon from './hasura_icon_dark.svg';
import styles from './Onboarding.module.scss';
import { ConsoleState } from '@hasura/metadata/api';
import { useCatalogState } from '../../../telemetry';
import { useAppContext } from '@hasura/shared/context';
import type { Metadata } from '@hasura/shared/types';
import { LS_KEYS } from '@hasura/shared/types';
import { Flex } from '@radix-ui/themes';
import { isMetadataEmpty } from '@hasura/metadata/helpers';

type PopupLinkProps = {
  title: string;
  index: number;
  link?: {
    pro: string;
    cloud: string;
    oss: string;
  };
  internalLink?: string;
  videoId?: string;
};

const PopupLink = ({
  link,
  index,
  videoId,
  title,
  internalLink,
}: PopupLinkProps) => {
  const { serverVersion } = useAppContext();
  if (videoId) {
    return (
      <li className={`${styles.popup_item} ${styles.video}`}>
        <div className={styles.link_container}>
          <span className={styles.link_num}>{index}</span>
          {title}
        </div>
        <YouTube videoId={videoId} opts={{ width: '100%', height: '240px' }} />
      </li>
    );
  }
  let url = link?.oss;
  if (serverVersion.includes('pro')) {
    url = link?.pro;
  } else if (serverVersion.includes('cloud')) {
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
            <FaDatabase />
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

          <FaExternalLinkAlt
            className={`${styles.link_icon} ${styles.muted}`}
          />
        </a>
      )}
    </li>
  );
};

const connectDatabaseHelper = {
  title: ' Connect Your First Database',
  internalLink: '/data/manage',
};

const onboardingList = [
  {
    id: 'getting-started-docs',
    title: 'Read the Getting Started Docs',
    link: {
      pro: 'https://hasura.io/docs/latest/graphql/core/getting-started/first-graphql-query.html?pg=pro&plcmt=onboarding-checklist#create-a-table',
      oss: 'https://hasura.io/docs/latest/graphql/core/getting-started/first-graphql-query.html?pg=oss-console&plcmt=onboarding#create-a-table',
      cloud:
        'https://hasura.io/docs/latest/graphql/core/getting-started/first-graphql-query.html?pg=cloud&plcmt=onboarding-checklist#create-a-table',
    },
  },
  {
    id: 'getting-started-video',
    title: 'Watch Our Getting Started Video',
    videoId: 'ZGKQ0U18USU',
  },
  {
    id: 'learn-courses',
    title: 'Bookmark Our Course',
    link: {
      pro: 'https://hasura.io/learn/graphql/hasura-advanced/introduction/?pg=pro&plcmt=onboarding-checklist',
      oss: 'https://hasura.io/learn/graphql/hasura/introduction/?pg=oss-console&plcmt=onboarding-checklist',
      cloud:
        'https://hasura.io/learn/graphql/hasura/introduction/?pg=cloud&plcmt=onboarding-checklist',
    },
  },
];

interface OnboardingProps {
  console_opts: ConsoleState | null;
  metadata: Metadata['metadata'] | undefined;
}

const Onboarding: React.FC<OnboardingProps> = ({ console_opts, metadata }) => {
  const { envVars } = useAppContext();
  const { setOnboardingCompleted } = useCatalogState();
  const [visible, setVisible] = React.useState(true);

  const shouldShowOnbaording = useMemo(() => {
    const shown = console_opts?.onboardingShown;
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

  // Only show onboarding popup on bottom right if environment is not cloud console
  if (isCloudConsole(envVars)) {
    return null;
  }

  const togglePopup = () => {
    setVisible((pre) => {
      setLSItem(LS_KEYS.showConsoleOnboarding, (!pre).toString());
      return !pre;
    });
  };

  const markCompleted = () => {
    setOnboardingCompleted();
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
              {metadata && !metadata.sources.length ? (
                <PopupLink {...connectDatabaseHelper} index={0} />
              ) : null}
              {onboardingList.map((item, i) => (
                <PopupLink {...item} key={item.id} index={i + 1} />
              ))}
            </ul>
          </div>
          <Separator size="4" />
          <Flex gap="2" justify="between" className="p-4">
            <Button
              onClick={togglePopup}
              data-test="btn-hide-for-now"
              color="gray"
            >
              Hide for now
            </Button>
            <Button
              onClick={markCompleted}
              data-test="btn-ob-dont-show-again"
              color="gray"
            >
              Don&apos;t show me again
            </Button>
          </Flex>
        </div>
      )}
    </>
  );
};

export default Onboarding;
