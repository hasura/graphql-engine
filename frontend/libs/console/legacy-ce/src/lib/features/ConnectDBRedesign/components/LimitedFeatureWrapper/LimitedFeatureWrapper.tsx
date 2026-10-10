import { isProConsole } from '@hasura/shared/utils';
import { EETrialCard, useEELiteAccess } from '../../../EETrial';
import { useAppContext } from '@hasura/shared/context';

export const LimitedFeatureWrapper = ({
  children,
  title,
  description,
  id,
  override,
}: //
{
  title: string;
  description: string;
  id: string;
  children?: React.ReactNode;
  override?: boolean;
}) => {
  const { envVars } = useAppContext();
  const { access: eeLiteAccess } = useEELiteAccess();

  /**
   * This boolean will override any checks
   */
  if (override) return <div>{children}</div>;

  /**
   * There are three cases here.
   * 1. If it's OSS - do not show the children at all. (there is no point in using this wrapper for oss features)
   * 2. If it's pro lite
   *   - show the "Try pro-lite" license form if license is not active.
   *   - show the children if license is active.
   * 3. If it's cloud/pro just show the children
   *
   */

  // this will tell us if console is pro or cloud
  const isPro = isProConsole(envVars);

  if (eeLiteAccess === 'active' || isPro) return <div>{children}</div>;

  // this is to return nothing for oss
  if (eeLiteAccess === 'forbidden') return null;

  return (
    <EETrialCard
      id={id}
      cardTitle={title}
      cardText={description}
      buttonType="default"
      eeAccess={eeLiteAccess}
      horizontal
    />
  );
};
