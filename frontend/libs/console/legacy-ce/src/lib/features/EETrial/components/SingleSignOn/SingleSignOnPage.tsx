import { Em, Flex, Heading, Link, Strong } from '@radix-ui/themes';
import { SingleSignOnSvg } from './SingleSignOnSvg';
import { useEELiteAccess } from '../../hooks/useEELiteAccess';
import { EETrialCard } from '../EETrialCard/EETrialCard';
import { Text } from '@hasura/shared/ui';

export const SingleSignOnPage = () => {
  const { access, consoleType } = useEELiteAccess();

  return (
    <Flex className="max-w-(--breakpoint-lg) p-4">
      <div>
        <Heading size="6">Single Sign On (SSO)</Heading>
        <div className="my-2">
          <Text color="gray">
            Enable secure organization access to manage your Hasura instance by
            integrating with single sign-on (SSO)
          </Text>{' '}
          <Link
            size="1"
            href="https://hasura.io/docs/latest/hasura-cloud/sso/"
            target="_blank"
            rel="noopener noreferrer"
          >
            <Em>(Know More)</Em>
          </Link>
        </div>
        <SingleSignOnSvg className="w-full mb-4" />
        {access === 'active' ||
        consoleType === 'cloud' ||
        consoleType === 'pro' ? (
          <div className="mt-4 text-muted">
            <Text color="gray">
              <Strong>Setup Single Sign-On (SSO)</Strong>
              <br />
              <Link
                target="_blank"
                href="https://hasura.io/docs/latest/hasura-cloud/sso/"
              >
                Read more
              </Link>{' '}
              on setting up multiple single sign-on (SSO) for your Hasura
              instance and your organization.
            </Text>
          </div>
        ) : (
          <EETrialCard
            className="mt-4"
            id="sso"
            cardTitle="Looking to secure your instance with single sign-on?"
            cardText={
              <span>
                Get production-ready today with a 30-day free trial of Hasura
                EE, no credit card required.
              </span>
            }
            buttonLabel="Enable Enterprise"
            eeAccess={access}
            horizontal
          />
        )}
      </div>
    </Flex>
  );
};
