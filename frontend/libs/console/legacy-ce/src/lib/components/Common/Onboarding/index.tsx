import React, { useMemo } from 'react';
import { FaExternalLinkAlt, FaDatabase } from 'react-icons/fa';
import {
  Button,
  HasuraIcon,
  Link,
  RelativeLink,
  Separator,
} from '@hasura/shared/ui';
import YouTube from 'react-youtube';
import { getLSItem, setLSItem, isCloudConsole } from '@hasura/shared/utils';
import { ConsoleState } from '@hasura/metadata/api';
import { useCatalogState } from '../../../telemetry';
import { useAppContext } from '@hasura/shared/context';
import type { Metadata } from '@hasura/shared/types';
import { LS_KEYS } from '@hasura/shared/types';
import { Card, Flex, Text } from '@radix-ui/themes';
import { isMetadataEmpty } from '@hasura/metadata/helpers';

// Styling uses Radix tokens only, so the popup follows the console appearance.
const badgeClassName =
  'inline-flex shrink-0 items-center justify-center w-6 h-6 rounded-full text-xs font-semibold';

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

const StepNumber = ({ index }: { index: number }) => (
  <span className={`${badgeClassName} bg-(--gray-a4) text-(--gray-12)`}>
    {index}
  </span>
);

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
      <li>
        <Flex align="center" gap="2" p="4">
          <StepNumber index={index} />
          <Text size="3">{title}</Text>
        </Flex>
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
    <li className="border-b border-(--gray-a5)">
      {internalLink ? (
        <RelativeLink
          to={internalLink}
          size="3"
          color="gray"
          highContrast
          underline="hover"
          className="flex items-center gap-2 p-4"
        >
          <span className={`${badgeClassName} bg-(--gray-12) text-(--gray-1)`}>
            <FaDatabase />
          </span>
          {title}
        </RelativeLink>
      ) : (
        <Link
          href={url}
          target="_blank"
          rel="noopener noreferrer"
          size="3"
          color="gray"
          highContrast
          underline="hover"
          className="flex items-center gap-2 p-4"
        >
          <StepNumber index={index} />
          <span className="flex-1">{title}</span>
          <FaExternalLinkAlt className="text-xs text-(--gray-a9)" />
        </Link>
      )}
    </li>
  );
};

const connectDatabaseHelper = {
  title: 'Connect Your First Database',
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
  const [visible, setVisible] = React.useState(() => {
    const show = getLSItem(LS_KEYS.showConsoleOnboarding) || 'true';

    return show === 'true';
  });

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

  if (!visible) {
    return (
      <button
        type="button"
        onClick={togglePopup}
        title="Get started with Hasura"
        className="fixed bottom-6 right-6 z-10 flex items-center justify-center w-12 h-12 rounded-full text-2xl cursor-pointer bg-(--color-panel-solid) shadow-(--shadow-3)"
      >
        <span aria-label="Wave" role="img">
          👋
        </span>
      </button>
    );
  }

  return (
    <Card
      size="1"
      className="fixed bottom-8 right-8 z-10 w-90 p-0! overflow-hidden shadow-(--shadow-4)"
      data-test="onboarding-popup"
    >
      {/* Brand yellow in both appearances; amber-contrast keeps text legible. */}
      <Flex
        align="center"
        gap="2"
        p="4"
        className="bg-(--amber-9) text-(--amber-contrast)"
        data-test="onboarding-popup-header"
      >
        <HasuraIcon className="w-6 h-6 shrink-0" aria-label="Hasura Logo" />
        <Text size="3" weight="bold">
          Hi there, let&apos;s get started with Hasura!
        </Text>
      </Flex>
      <ul className="list-none p-0 m-0">
        {metadata && !metadata.sources.length ? (
          <PopupLink {...connectDatabaseHelper} index={0} />
        ) : null}
        {onboardingList.map((item, i) => (
          <PopupLink {...item} key={item.id} index={i + 1} />
        ))}
      </ul>
      <Separator size="4" />
      <Flex gap="2" justify="between" p="4">
        <Button onClick={togglePopup} data-test="btn-hide-for-now" color="gray">
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
    </Card>
  );
};

export default Onboarding;
