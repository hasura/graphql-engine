import * as React from 'react';
import { Button } from '../../../new-components/Button';
import globals from '../../../Globals';
import { FaStar, FaTimesCircle } from 'react-icons/fa';
import { useEELiteAccess } from '../hooks/useEELiteAccess';
import { EELiteAccess } from '../types';
import { WithEEBenefits } from './BenefitsView/WithEEBenefits';
import { getDaysFromNow } from '../utils';
import { EnableEEButtonWrapper } from './EnableEnterpriseButton';
import { Analytics } from '../../Analytics';

export const NavbarButton: React.VFC<{
  className?: string;
  globals: typeof globals;
}> = props => {
  const eeLite = useEELiteAccess(globals);
  const { access } = eeLite;

  if (access === 'forbidden') {
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

export const EnterpriseButton: React.VFC<ButtonProps> = props => {
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
export const EEButton: React.FC<EEButtonProps> = props => {
  const { kind } = props;
  switch (kind) {
    case 'active': {
      const { text } = props;
      return (
        <Analytics name="ee-navbar-button" passHtmlAttributesToChildren>
          <Button
            mode="default"
            size="md"
            className="bg-none text-current pointer-effects-none bg-transparent border-primary hover:border-primary text-primary text-sm hover:brightness-90"
            icon={<FaStar className="text-primary" />}
          >
            <span className="text-primary font-semibold text-lg">{text}</span>
          </Button>
        </Analytics>
      );
    }
    case 'inactive': {
      const { primaryText, secondaryText } = props;
      return (
        <Analytics name="ee-navbar-button" passHtmlAttributesToChildren>
          <Button
            mode="default"
            size="md"
            className={
              'bg-none pointer-effects-none bg-transparent border-slate-400 text-sm'
            }
            icon={<FaTimesCircle className="text-slate-400" />}
          >
            <span className="font-semibold text-slate-400 text-lg uppercase">
              {primaryText}
            </span>
            &nbsp;
            <span className="font-normal text-slate-400 text-lg">
              {secondaryText}
            </span>
          </Button>
        </Analytics>
      );
    }
    case 'loading': {
      const { text } = props;
      return (
        <Analytics name="ee-navbar-button" passHtmlAttributesToChildren>
          <Button
            mode="default"
            size="md"
            disabled
            loadingText={
              <span className="text-primary font-semibold text-lg">{text}</span>
            }
            isLoading
            className="bg-none bg-transparent border-none border-primary text-primary text-lg"
          >
            {text}
          </Button>
        </Analytics>
      );
    }
    default:
      return null;
  }
};
