import React, { useEffect, useState, useMemo } from 'react';
import { Button } from '../../../../../new-components/Button';
import { LS_KEYS, removeLSItem } from '../../../../../utils/localStorage';
import dataApiCard from './images/data-api.svg';
import dataFederationCard from './images/data-sources.svg';
import gatewayCard from './images/gateway.svg';
import gqlBackend from './images/gql-backend.svg';
import hasuraIllustrations from './images/hasura-illustration.svg';
import { emitOnboardingEvent, persistSkippedOnboarding } from '../../utils';
import { getUseCaseExperimentOnboardingVariables } from '../../constants';
import { Analytics, trackCustomEvent } from '../../../../Analytics';
import { Dispatch } from '../../../../../types';
import _push from '../../../../../components/Services/Data/push';

type UseCaseScreenProps = {
  dismiss: () => void;
  dispatch: Dispatch;
};

export type UseCases =
  | 'data-api'
  | 'gql-backend'
  | 'data-federation'
  | 'gateway';

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
    image: dataApiCard,
    title: 'Data API',
    description:
      'Build an instant, real-time API over your data sources for easy and performant access',
    consoleUrl: '/',
    docsUrl: 'https://hasura.io/docs/latest/resources/use-case/data-api/',
  },
  {
    id: 'gql-backend',
    image: gqlBackend,
    title: 'Graphql Backend',
    description:
      'Build a lightning-fast GraphQL backend and significantly accelerate your application development',
    consoleUrl: '/',
    docsUrl: 'https://hasura.io/docs/latest/resources/use-case/gql-backend/',
  },
  {
    id: 'data-federation',
    image: dataFederationCard,
    title: 'Data Federation',
    description:
      'Build an API that enables real-time data composition from different data sources',
    consoleUrl: '/',
    docsUrl:
      'https://hasura.io/docs/latest/resources/use-case/data-federation/',
  },
  {
    id: 'gateway',
    image: gatewayCard,
    title: 'Gateway Service',
    description:
      'Build a single entry point from client applications into an ecosystem of microservices',
    consoleUrl: '/',
    docsUrl: 'https://hasura.io/docs/latest/resources/use-case/api-gateway/',
  },
];

export const UseCaseScreen = (props: UseCaseScreenProps) => {
  const [selectedUseCase, setSelectedUseCase] = useState<UseCases | null>(null);

  useEffect(() => {
    trackCustomEvent({
      location: 'Console',
      action: 'Load',
      object: 'Use Case Wizard',
    });
  }, []);

  const randomUseCaseAssets = useMemo(() => {
    return useCasesAssets.sort(() => Math.random() - 0.5);
  }, [useCasesAssets]);

  const onSubmit = () => {
    const useCase = useCasesAssets.filter(
      useCase => useCase.id === selectedUseCase
    )[0];

    removeLSItem(LS_KEYS.useCaseExperimentOnboarding);
    props.dispatch(_push(useCase.consoleUrl));
    window.open(useCase.docsUrl, '_blank', 'noreferrer,noopener');
    emitOnboardingEvent(getUseCaseExperimentOnboardingVariables(useCase.id));
  };

  return (
    <div
      className="use-case-container border border-solid border-gray-400 rounded-sm flex flex-col flex-wrap p-8 h-full bg-white"
      style={{
        width: '902px',
      }}
    >
      <div className="use-case-welcome-header flex justify-between w-full">
        <div className="use-case-welcome-text font-sans">
          <h1 className="text-xl font-bold text-cloud-darkest">
            Welcome to Hasura!
          </h1>
          <div className="text-muted-dark font-normal">
            Hasura can help you supercharge your development
          </div>
        </div>
        <div className="use-case-welcome-illustrations">
          <img src={hasuraIllustrations} alt="welcome" />
        </div>
      </div>
      <div className="use-case-intro-text text-[#64748B] font-sans mt-3 mb-3.5">
        What would you like to build with Hasura?
      </div>
      <div className="use-cases flex flex-wrap justify-between gap-y-15 gap-y-8">
        {randomUseCaseAssets.map((item, index) => (
          <label
            key={index}
            htmlFor={item.id}
            className="use-case-card flex flex-col cursor-pointer border border-solid border-slate-300 rounded focus-within:border focus-within:border-solid focus-within:border-amber-500 transition-shadow shadow-none hover:shadow-md w-[400px]"
            onChange={event => {
              const selectedUseCaseNode = event.target as HTMLInputElement;
              setSelectedUseCase(selectedUseCaseNode.id as UseCases);
            }}
          >
            <img src={item.image} alt={item.id} />
            <div className="data-api-description flex mt-3 ml-6 mb-2">
              <input
                type="radio"
                id={item.id}
                name="use-case"
                className="mt-1"
              />
              <div className="flex flex-col font-sans ml-2">
                <div className="font-semibold text-slate-900">{item.title}</div>
                <div className="text-muted-dark font-normal">
                  {item.description}
                </div>
              </div>
            </div>
          </label>
        ))}
      </div>
      <div className="use-case-cta flex justify-between w-full mt-8">
        <Analytics name="use-case-onboarding-skip">
          <div
            className="ml-xs mr-4 text-secondary flex items-center cursor-pointer"
            onClick={() => {
              props.dismiss();
              removeLSItem(LS_KEYS.useCaseExperimentOnboarding);
              persistSkippedOnboarding();
            }}
          >
            Skip
          </div>
        </Analytics>
        <Analytics
          name={`use-case-onboarding-${selectedUseCase}`}
          passHtmlAttributesToChildren
        >
          <Button
            mode="primary"
            disabled={selectedUseCase === null}
            onClick={() => {
              props.dismiss();
              onSubmit();
            }}
          >
            Continue
          </Button>
        </Analytics>
      </div>
    </div>
  );
};
