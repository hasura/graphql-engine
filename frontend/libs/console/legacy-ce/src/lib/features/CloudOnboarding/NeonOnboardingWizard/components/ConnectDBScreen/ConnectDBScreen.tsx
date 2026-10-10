import { useEffect } from 'react';
import { Flex } from '@radix-ui/themes';

import { OnboardingAnimation } from './components/OnboardingAnimation';
import { NeonOnboarding } from './components/NeonOnboarding';
import { trackCustomEvent } from '@hasura/shared/analytics';

type ConnectDBScreenProps = {
  proceed: VoidFunction;
  dismissOnboarding: VoidFunction;
  setStepperIndex: (index: number) => void;
};

export function ConnectDBScreen(props: ConnectDBScreenProps) {
  useEffect(() => {
    trackCustomEvent({
      location: 'Console',
      action: 'Load',
      object: 'Neon Onboarding Wizard',
    });
  }, []);
  const { proceed, dismissOnboarding, setStepperIndex } = props;

  return (
    <>
      <OnboardingAnimation />
      <Flex align="center" justify="between">
        <NeonOnboarding
          dismiss={dismissOnboarding}
          proceed={proceed}
          setStepperIndex={setStepperIndex}
        />
      </Flex>
    </>
  );
}
