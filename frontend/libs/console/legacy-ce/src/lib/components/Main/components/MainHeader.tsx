import { Link, Location } from 'react-router';
import styles from '../Main.module.scss';
import HeaderNavItem, {
  activeLinkStyle,
  itemContainerStyle,
  linkStyle,
} from './HeaderNavItem';
import {
  FaCloud,
  FaCog,
  FaCogs,
  FaDatabase,
  FaExclamationCircle,
  FaExclamationTriangle,
  FaFlask,
  FaInfoCircle,
  FaPlug,
} from 'react-icons/fa';
import * as tooltips from './Tooltips';
import clsx from 'clsx';
import React, { useState } from 'react';
import { getProClickState, setProClickState } from '../utils';
import {
  Badge,
  HasuraEELogo,
  HasuraLogo,
  Text,
  Tooltip,
} from '@hasura/shared/ui';
import { getPathRoot } from '@hasura/shared/utils';
import { ProPopup } from './ProPopup';
import { Help } from './Help';
import NotificationSection from './NotificationSection';
import UserMenu from './UserMenu';
import { Flex } from '@radix-ui/themes';
import { useAppContext } from '@hasura/shared/context';
import { CloudAccessState, getConsolidatedPath } from '../../../shared/auth';

type Props = {
  serverVersion: string;
  location: Location;
  isConsistentMetadata: boolean;
  moreLeftItems?: React.ReactNode;
  moreRightItems?: React.ReactNode;
  moreUserMenuItems?: React.ReactElement[];
  accesses: CloudAccessState;
};

const MainHeader = ({
  serverVersion,
  location,
  isConsistentMetadata,
  moreLeftItems,
  moreRightItems,
  moreUserMenuItems,
  accesses,
}: Props) => {
  const { envVars } = useAppContext();
  const [isPopUpOpen, setIsPopUpOpen] = useState(false);

  const isAdminSecretSet = Boolean(envVars.isAdminSecretSet);

  const toggleProPopup = () => {
    setIsPopUpOpen(!isPopUpOpen);
  };

  function updateLocalStorageState() {
    const s = getProClickState();
    if (s && 'isProClicked' in s && !s.isProClicked) {
      setProClickState({
        isProClicked: !s.isProClicked,
      });
    }
  }

  const onProIconClick = () => {
    updateLocalStorageState();
    toggleProPopup();
  };

  const getAdminSecretSection = () => {
    if (!isAdminSecretSet) {
      return (
        <Tooltip
          side="bottom"
          content={`This graphql endpoint is public and you should add an x-hasura-admin-secret`}
        >
          <div className={itemContainerStyle}>
            <a
              className={linkStyle}
              href="https://hasura.io/docs/latest/deployment/securing-graphql-endpoint/"
              target="_blank"
              rel="noopener noreferrer"
            >
              <Badge color="amber" variant="solid">
                <FaExclamationTriangle />
                &nbsp;Secure your endpoint
              </Badge>
            </a>
          </div>
        </Tooltip>
      );
    }

    return null;
  };

  const getMetadataStatusIcon = () => {
    const cogIcon = <FaCog className="w-3 h-3" />;
    if (isConsistentMetadata) {
      return cogIcon;
    }

    return (
      <div className="relative">
        {cogIcon}
        <div className="absolute -top-2 left-2 ">
          <FaExclamationCircle
            className="bg-white rounded-full"
            color="#d9534f"
          />
        </div>
      </div>
    );
  };

  const currentActiveBlock = getPathRoot(location.pathname);
  const logo =
    envVars.consoleType === 'pro-lite' || envVars.consoleType === 'pro' ? (
      <HasuraEELogo className="w-24" />
    ) : (
      <HasuraLogo className="w-24" />
    );

  return (
    <Flex className="font-sans bg-slate-700 dark:bg-slate-900 text-slate-100 h-16">
      <Flex gap="1" className="grow">
        <Flex align="center" gap="3" className="px-5 py-2">
          <Link to="/">{logo}</Link>
          <Link to="/">
            <div className="text-white text-xs max-w-[128px]">
              {serverVersion}
            </div>
          </Link>
        </Flex>
        <Flex gap="2" data-testid="Nav bar" align="center">
          <HeaderNavItem
            title="API"
            icon={FaFlask}
            tooltipText={tooltips.apiExplorer}
            path={getConsolidatedPath(accesses, '/api/api-explorer', '/api')}
            pathname={location.pathname}
            isDefault
          />
          <HeaderNavItem
            title="Data"
            icon={FaDatabase}
            tooltipText={tooltips.data}
            path={getConsolidatedPath(accesses, '/data', '/data')}
            pathname={location.pathname}
          />
          <HeaderNavItem
            title="Actions"
            icon={FaCogs}
            tooltipText={tooltips.actions}
            path={getConsolidatedPath(
              accesses,
              '/actions/manage/actions',
              '/actions',
            )}
            pathname={location.pathname}
          />
          <HeaderNavItem
            title="Remote Schemas"
            icon={FaPlug}
            tooltipText={tooltips.remoteSchema}
            path={getConsolidatedPath(
              accesses,
              '/remote-schemas/manage/schemas',
              '/remote-schemas',
            )}
            pathname={location.pathname}
          />
          <HeaderNavItem
            title="Events"
            icon={FaCloud}
            tooltipText={tooltips.events}
            path={getConsolidatedPath(
              accesses,
              '/events/data/manage',
              '/events',
            )}
            pathname={location.pathname}
          />
          {moreLeftItems}
        </Flex>
      </Flex>
      <Flex
        id="dropdown_wrapper"
        gap="2"
        justify="end"
        align="center"
        className={'relative mr-4'}
      >
        {getAdminSecretSection()}
        {moreRightItems}
        <div className={itemContainerStyle}>
          <Link
            className={clsx(
              linkStyle,
              currentActiveBlock === 'settings' && activeLinkStyle,
            )}
            to={getConsolidatedPath(accesses, '/settings', '/settings')}
          >
            <span className="text-sm self-baseline">
              {getMetadataStatusIcon()}
            </span>
            <Text size="1" className="uppercase text-left">
              Settings
            </Text>
          </Link>
        </div>
        {/* Compensate legacy styles directly with style attribute */}
        {envVars.consoleType === 'oss' && (
          <div className={styles.proWrapper} style={{ padding: '0' }}>
            <div className={itemContainerStyle}>
              <div
                className={clsx(linkStyle, isPopUpOpen && activeLinkStyle)}
                onClick={onProIconClick}
              >
                <span className="text-sm self-baseline">
                  <FaInfoCircle className="w-3 h-3" />
                </span>
                <Text size="1" className="uppercase text-left">
                  CLOUD
                </Text>
              </div>
            </div>
            {isPopUpOpen && <ProPopup toggleOpen={toggleProPopup} />}
          </div>
        )}

        <Help isSelected={currentActiveBlock === 'support'} />
        {serverVersion &&
        accesses &&
        'hasDataAccess' in accesses &&
        accesses.hasDataAccess ? (
          <NotificationSection />
        ) : null}
        <UserMenu items={moreUserMenuItems} />
      </Flex>
    </Flex>
  );
};

export default MainHeader;
