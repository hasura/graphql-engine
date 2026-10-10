import React from 'react';
import { IconType } from 'react-icons';
import { Flex, Heading } from '@radix-ui/themes';
import { Badge, Button, Separator } from '@hasura/shared/ui';

import {
  FaClock,
  FaCopy,
  FaDatabase,
  FaExternalLinkAlt,
  FaShieldAlt,
  // FaUsers,
  FaTimesCircle,
} from 'react-icons/fa';
import PrometheusLogo from '../../assets/logo-prometheus.svg';
import IconOpenTelemetry from '../../assets/icon-opentelemetry.svg';
import IconQueryCaching from '../../assets/icon-query-caching.png';
import { ListItem } from './ListItem';
import { ListHeader } from './ListHeader';
import { EELicenseInfo } from '../../types';
import { getDaysFromNow } from '../../utils';
import { EE_TRIAL_CONTACT_US_URL } from '../../constants';
import { Analytics } from '@hasura/shared/analytics';

type EETrialBenefitsProps = {
  licenseInfo: EELicenseInfo;
};

export const BenefitsView = (props: EETrialBenefitsProps) => {
  const {
    licenseInfo: { status },
  } = props;
  const badgeColor = status === 'active' ? 'indigo' : 'red';

  let expirationMessage = '';
  let expiryBannerBadge: React.ReactNode = null;
  let ctaButtonText = 'Enable Enterprise';
  let ctaButtonIcon: IconType | undefined;

  const benefitsHeaderText = 'Benefits of Hasura Enterprise Edition';
  switch (props.licenseInfo.status) {
    case 'active': {
      const expiryDaysFromNow = Math.abs(
        getDaysFromNow(props.licenseInfo.expiry_at),
      );
      const expiryDaysText =
        expiryDaysFromNow === 1
          ? `${expiryDaysFromNow} day`
          : `${expiryDaysFromNow} days`;
      expiryBannerBadge = <FaClock className="mr-1" />;
      expirationMessage = `Your Enterprise license is expiring in ${expiryDaysText}.`;
      ctaButtonText = 'Renew License';
      ctaButtonIcon = FaExternalLinkAlt;
      break;
    }
    case 'deactivated': {
      expirationMessage = `Your Enterprise license has been deactivated. Please get in touch.`;
      expiryBannerBadge = <FaTimesCircle className="mr-1" />;
      ctaButtonText = 'Get In Touch';
      ctaButtonIcon = FaExternalLinkAlt;
      break;
    }
    case 'expired': {
      expirationMessage = `Your Enterprise license has expired`;
      expiryBannerBadge = <FaTimesCircle className="mr-1" />;
      ctaButtonText = 'Renew License';
      ctaButtonIcon = FaExternalLinkAlt;
      break;
    }
    case 'none':
    default:
      expirationMessage = '';
      ctaButtonText = 'Get License';
      ctaButtonIcon = FaExternalLinkAlt;
      expiryBannerBadge = null;
      break;
  }

  return (
    <>
      <Flex direction="column" gap="3" className="p-4">
        <div className="mb-4">
          {expirationMessage && (
            <Badge className="mb-1 text-xs" color={badgeColor}>
              {expiryBannerBadge}
              <div>{expirationMessage}</div>
            </Badge>
          )}
          <Heading size="6">{benefitsHeaderText}</Heading>
        </div>
        <ListHeader label="OBSERVABILITY" />
        <ListItem
          id="prometheus-metrics"
          label="Prometheus Metrics"
          icon={PrometheusLogo}
          url="https://hasura.io/docs/2.0/observability/enterprise-edition/prometheus/metrics/"
        />
        <ListItem
          label="OpenTelemetry Exporter"
          id="opentlemetry-exporter"
          icon={IconOpenTelemetry}
          url="https://hasura.io/docs/2.0/observability/opentelemetry/"
        />
        <ListHeader label="PERFORMANCE" />
        <ListItem
          label="Query Response Caching"
          id="query-response-caching"
          icon={IconQueryCaching}
          url="https://hasura.io/docs/latest/enterprise/caching/"
        />
        <ListItem
          label="Read Replicas"
          id="read-replicas"
          icon={<FaCopy />}
          url="https://hasura.io/docs/latest/databases/database-config/read-replicas/"
        />
        {/*        <ListItem
          label="Dynamic Database Queries (Tenancy, Read Consistency)"
          icon={<FaDatabase />}
          url="https://hasura.io/docs/latest/databases/connect-db/dynamic-db-connection/"
        />
*/}
        <ListHeader label="DATA SOURCE CONNECTORS" />
        <ListItem
          label="Snowflake, Athena, and more..."
          id="data-source-connectors"
          icon={<FaDatabase />}
          url="https://hasura.io/docs/latest/databases/overview/"
        />
        <ListHeader label="ENHANCED SECURITY" />
        {/*        <ListItem
          label="Console Single Sign-On"
          icon={<FaUsers />}
          url="https://hasura.io/docs/latest/hasura-cloud/sso/"
        />
*/}{' '}
        <ListItem
          label="Role-Based Operation Allow List"
          id="role-based-allow-list"
          icon={<FaShieldAlt />}
          url="https://hasura.io/docs/latest/security/allow-list/#role-based-allow-list"
        />
        <ListItem
          label="Rate Limiting"
          id="rate-limiting"
          icon={<FaShieldAlt />}
          url="https://hasura.io/docs/latest/queries/response-caching/#rate-limiting"
        />
        <ListItem
          id="introspection-disabling"
          label="Introspection Disabling"
          icon={<FaShieldAlt />}
          url="https://hasura.io/docs/latest/security/disable-graphql-introspection/"
        />
      </Flex>
      <Separator size="4" />
      <Flex justify="end" className="gap-4 mt-4">
        <Analytics name="ee-benefits-view-contact-us">
          <a
            href={EE_TRIAL_CONTACT_US_URL}
            target="_blank"
            rel="noreferrer noopener"
          >
            <Button rightIcon={ctaButtonIcon} mode="primary">
              {ctaButtonText}
            </Button>
          </a>
        </Analytics>
      </Flex>
    </>
  );
};
