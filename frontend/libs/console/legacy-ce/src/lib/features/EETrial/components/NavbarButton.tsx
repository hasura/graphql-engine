import * as React from 'react';
import { Button, useAppearance } from '@hasura/shared/ui';
import { FaStar, FaTimesCircle } from 'react-icons/fa';
import { useEELiteAccess } from '../hooks/useEELiteAccess';
import { EELiteAccess } from '../types';
import { WithEEBenefits } from './BenefitsView/WithEEBenefits';
import { getDaysFromNow } from '../utils';
import { EnableEEButtonWrapper } from './EnableEnterpriseButton';
import { Analytics } from '@hasura/shared/analytics';

export const NavbarButton: React.FC<{
  className?: string;
}> = (props) => {
  const eeLite = useEELiteAccess();
  const { access } = eeLite;

  if (access !== 'active') {
    return null;
  }

  return (
    <div className={props.className}>
      <EnterpriseButton accessInfo={eeLite} />
    </div>
  );
};

type ButtonProps = {
  accessInfo: EELiteAccess;
};

export const EnterpriseButton: React.FC<ButtonProps> = (props) => {
  const { accessInfo } = props;

  switch (accessInfo.access) {
    case 'active': {
      switch (accessInfo.kind) {
        case 'grace': {
          return (
            <WithEEBenefits id="navbar-ee-button">
              <EEButton kind="active" text="EE (Expired)" />
            </WithEEBenefits>
          );
        }
        case 'default':
        default: {
          const daysFromNow = Math.abs(getDaysFromNow(accessInfo.expires_at));
          const daysFromNowDisplayText =
            daysFromNow === 1 ? `${daysFromNow} day` : `${daysFromNow} days`;
          return (
            <WithEEBenefits id="navbar-ee-button">
              <EEButton kind="active" text={`EE (${daysFromNowDisplayText})`} />
            </WithEEBenefits>
          );
        }
      }
    }
    case 'expired': {
      return (
        <WithEEBenefits id="navbar-ee-button">
          <EEButton
            kind="inactive"
            primaryText="EE"
            secondaryText="(Expired)"
          />
        </WithEEBenefits>
      );
    }
    case 'deactivated': {
      return (
        <WithEEBenefits id="navbar-ee-button">
          <EEButton
            kind="inactive"
            primaryText="EE"
            secondaryText="(Deactivated)"
          />
        </WithEEBenefits>
      );
    }

    case 'eligible': {
      return (
        <EnableEEButtonWrapper>
          <EEButton kind="active" text="ENTERPRISE" />
        </EnableEEButtonWrapper>
      );
    }
    case 'forbidden':
    case 'loading':
    default: {
      return null;
    }
  }
};

type EEButtonProps =
  | {
      kind: 'active';
      text: string;
    }
  | {
      kind: 'inactive';
      primaryText: string;
      secondaryText: string;
    }
  | {
      kind: 'loading';
      text: string;
    };
export const EEButton: React.FC<EEButtonProps> = (props) => {
  const { appearance } = useAppearance();

  switch (props.kind) {
    case 'active': {
      return (
        <Analytics name="ee-navbar-button" passHtmlAttributesToChildren>
          <Button
            color="amber"
            size="2"
            variant={appearance === 'dark' ? 'surface' : 'solid'}
            leftIcon={FaStar}
          >
            {props.text}
          </Button>
        </Analytics>
      );
    }
    case 'inactive': {
      const { primaryText, secondaryText } = props;
      return (
        <Analytics name="ee-navbar-button" passHtmlAttributesToChildren>
          <Button color="gray" size="2" leftIcon={FaTimesCircle}>
            {primaryText}
            &nbsp;
            {secondaryText}
          </Button>
        </Analytics>
      );
    }
    case 'loading': {
      const { text } = props;
      return (
        <Analytics name="ee-navbar-button" passHtmlAttributesToChildren>
          <Button size="md" disabled loadingText={text} loading>
            {text}
          </Button>
        </Analytics>
      );
    }
    default:
      return null;
  }
};
