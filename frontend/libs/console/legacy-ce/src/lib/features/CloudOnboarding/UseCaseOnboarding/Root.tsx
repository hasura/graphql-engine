import React, { useEffect, useState } from 'react';
import { Button } from '@hasura/shared/ui';
import { emitOnboardingEvent } from '../utils';
import {
  getUseCaseExperimentOnboardingVariables,
  skippedUseCaseExperimentOnboarding,
} from '../constants';
import { Analytics, trackCustomEvent } from '@hasura/shared/analytics';
import { DialogContainer } from '../components';
import { useNavigate } from 'react-router';
import { Flex } from '@radix-ui/themes';

type UseCaseScreenProps = {
  dismiss: () => void;
};

export type UseCases =
  'data-api' | 'gql-backend' | 'data-federation' | 'gateway';

interface UseCaseAssets {
  id: UseCases;
  title: string;
  description: string;
  image: string;
  consoleUrl: string;
  docsUrl: string;
}

const useCasesAssets: UseCaseAssets[] = [
  {
    id: 'data-api',
    image:
      'https://storage.googleapis.com/graphql-engine-cdn.hasura.io/cloud-console/assets/common/img/hasura-usecase-data-api.svg',
    title: 'Data Access Layer',
    description:
      'Build an instant, real-time API over your data sources for easy and performant access',
    consoleUrl: '/',
    docsUrl: 'https://hasura.io/docs/latest/resources/use-case/data-api/',
  },
  {
    id: 'gql-backend',
    image:
      'https://storage.googleapis.com/graphql-engine-cdn.hasura.io/cloud-console/assets/common/img/hasura-usecase-gql-backend.svg',
    title: 'Graphql Backend',
    description:
      'Build a lightning-fast GraphQL backend and significantly accelerate your application development',
    consoleUrl: '/',
    docsUrl: 'https://hasura.io/docs/latest/resources/use-case/gql-backend/',
  },
  {
    id: 'gateway',
    image:
      'https://storage.googleapis.com/graphql-engine-cdn.hasura.io/cloud-console/assets/common/img/hasura-usecase-gateway.svg',
    title: 'API Gateway',
    description:
      'Build a single entry point from client applications into an ecosystem of microservices',
    consoleUrl: '/',
    docsUrl: 'https://hasura.io/docs/latest/resources/use-case/api-gateway/',
  },
];

export const Root = (props: UseCaseScreenProps) => {
  const navigate = useNavigate();
  const [selectedUseCase, setSelectedUseCase] = useState<UseCases | null>(null);

  useEffect(() => {
    trackCustomEvent({
      location: 'Console',
      action: 'Load',
      object: 'Use Case Wizard',
    });
  }, []);

  const [randomUseCaseAssets] = useState(() =>
    [...useCasesAssets].sort(() => Math.random() - 0.5),
  );

  const useCase = useCasesAssets.find(
    (useCase) => useCase.id === selectedUseCase,
  );

  const onSubmit = () => {
    if (useCase) {
      navigate(useCase.consoleUrl);
      emitOnboardingEvent(getUseCaseExperimentOnboardingVariables(useCase.id));
      props.dismiss();
    }
  };

  return (
    <DialogContainer header={''}>
      <Flex
        direction="column"
        wrap="wrap"
        className="use-case-container border border-solid border-gray-400 rounded-sm p-8 h-full bg-white"
        style={{
          width: '902px',
        }}
      >
        <Flex justify="between" className="use-case-welcome-header w-full">
          <div className="use-case-welcome-text font-sans">
            <h1 className="text-xl font-bold text-cloud-darkest">
              Welcome to Hasura!
            </h1>
            <div className="text-muted-dark font-normal">
              Hasura can help you supercharge your development
            </div>
          </div>
          <div className="use-case-welcome-illustrations h-[93px]">
            <img
              src="https://storage.googleapis.com/graphql-engine-cdn.hasura.io/cloud-dashboard/dashboard/hasura-loading-illustration.svg"
              alt="welcome"
            />
          </div>
        </Flex>
        <div className="use-case-intro-text text-[#64748B] font-sans mt-3 mb-3.5">
          What would you like to build with Hasura?
        </div>
        <Flex wrap="wrap" className="use-cases gap-y-15 gap-y-8 justify-around">
          {randomUseCaseAssets.map((item, index) => (
            <Flex className="relative h-[250px]" key={index}>
              <label
                key={index}
                htmlFor={item.id}
                className="use-case-card flex flex-col cursor-pointer border border-solid border-slate-300 rounded focus-within:border focus-within:border-solid focus-within:border-amber-500 transition-shadow shadow-none hover:shadow-md w-[400px]"
                onChange={(event) => {
                  const selectedUseCaseNode =
                    event.target as EventTarget as HTMLInputElement;
                  setSelectedUseCase(selectedUseCaseNode.id as UseCases);
                }}
              >
                <img src={item.image} alt={item.id} />

                <Flex className="absolute bottom-0 data-api-description mt-3 ml-6 mb-2">
                  <input
                    type="radio"
                    id={item.id}
                    name="use-case"
                    className="mt-1"
                  />
                  <Flex direction="column" className="font-sans ml-2">
                    <div className="font-semibold text-slate-900">
                      {item.title}
                    </div>
                    <div className="text-muted-dark font-normal">
                      {item.description}
                    </div>
                  </Flex>
                </Flex>
              </label>
            </Flex>
          ))}
        </Flex>
        <Flex justify="between" className="use-case-cta w-full mt-8">
          <Analytics name="use-case-onboarding-skip">
            <Flex
              align="center"
              className="ml-xs mr-4 text-secondary cursor-pointer"
              onClick={() => {
                props.dismiss();
                emitOnboardingEvent(skippedUseCaseExperimentOnboarding);
              }}
            >
              Skip
            </Flex>
          </Analytics>
          {useCase ? (
            <Analytics name={`use-case-onboarding-${selectedUseCase}`}>
              <a
                href={useCase.docsUrl}
                target="_blank"
                rel="noopener noreferrer"
                onClick={onSubmit}
              >
                <Button mode="primary">Continue</Button>
              </a>
            </Analytics>
          ) : (
            <Analytics
              name={`use-case-onboarding-unselected`}
              passHtmlAttributesToChildren
            >
              <Button mode="primary" disabled>
                Continue
              </Button>
            </Analytics>
          )}
        </Flex>
      </Flex>
    </DialogContainer>
  );
};
