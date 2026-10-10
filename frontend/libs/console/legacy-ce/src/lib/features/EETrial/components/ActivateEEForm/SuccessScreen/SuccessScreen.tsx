import { Button, Separator, IconButton } from '@hasura/shared/ui';
import { FaArrowRight, FaCheck } from 'react-icons/fa';
import { Analytics } from '@hasura/shared/analytics';
import { Code, Em, Flex, Heading } from '@radix-ui/themes';

type Props = {
  /**
   * Show `View Benefits` button in the bottom right
   */
  showBenefitsButton?: boolean;
  /**
   * Callback for the action to be performed on `View Benefits` button click
   */
  onViewBenefitsClick?: VoidFunction;
  /**
   * Callback for the action to be performed on `Close and Continue` button click
   */
  onCloseClick?: VoidFunction;
};

export const SuccessScreen = (props: Props) => {
  const { showBenefitsButton, onViewBenefitsClick, onCloseClick } = props;
  return (
    <>
      <Flex direction="column" className="py-4" gap="2">
        <Flex align="center" justify="start" gap="2">
          <IconButton
            color="green"
            variant="outline"
            radius="full"
            className="cursor-none!"
          >
            <FaCheck />
          </IconButton>
          <Heading size="5">
            Your trial of Hasura Enterprise has been activated
          </Heading>
        </Flex>
        <p className="text-muted mt-0 mb-2">
          <strong>What&apos;s next?</strong>
          <br />
          Please restart your Hasura service in order to start using your new
          Hasura Enterprise features.
        </p>
        <p className="text-muted mt-0 mb-0">
          In Docker, you can restart your container using:
        </p>
        <Code color="red">
          <Em>docker restart [container-name]</Em>
        </Code>
        <p className="text-muted mt-0 mb-2">
          Read our{' '}
          <a
            href="https://hasura.io/docs/latest/enterprise/index"
            target="_blank"
            rel="noopener noreferrer"
            className="text-secondary font-semibold cursor-pointer"
          >
            Hasura Enterprise Edition documentation
          </a>{' '}
          to learn how to get the most out of the features of your trial.
        </p>
      </Flex>
      <Separator size="4" />
      <Flex justify="between" gap="4" className="py-3">
        {showBenefitsButton ? (
          <Button onClick={onViewBenefitsClick}>View Benefits</Button>
        ) : null}
        <Analytics name="ee-activation-success-close-and-continue">
          <Button
            mode="primary"
            onClick={onCloseClick}
            rightIcon={FaArrowRight}
          >
            Close and Continue
          </Button>
        </Analytics>
      </Flex>
    </>
  );
};
