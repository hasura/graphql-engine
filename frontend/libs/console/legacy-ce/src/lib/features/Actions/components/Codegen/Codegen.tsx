import React, { useEffect } from 'react';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { getSdlComplete } from '../../../../shared/utils/sdlUtils';
import {
  getStarterKitPath,
  getStarterKitDownloadPath,
  getGlitchProjectURL,
} from './utils';
import CodeTabs from './CodeTabs';
import DerivedFrom from './DerivedFrom';
import { getPersistedDerivedAction } from '../../utils';
import { BsDownload, BsGithub, BsLink } from 'react-icons/bs';
import { useCurrentActionContext } from '../../context';
import useGetAllCodegenFrameworks from '../../hooks/useGetAllCodegenFrameworks';
import {
  Button,
  IndicatorCard,
  Link,
  Select,
  SkeletonList,
  Text,
} from '@hasura/shared/ui';
import { Flex, Strong } from '@radix-ui/themes';

const Codegen = () => {
  const { currentAction, metadata } = useCurrentActionContext();

  useDocumentTitle(`Codegen - ${currentAction.name} - Actions | Hasura`);

  const [actionsSdl, setActionsSdl] = React.useState('');
  const [selectedFramework, selectFramework] = React.useState('');
  const [parentMutation] = React.useState(
    getPersistedDerivedAction(currentAction.name),
  );
  const [shouldDerive, setShouldDerive] = React.useState(true);
  const {
    data: allFrameworks,
    isFetching,
    error,
    refetch,
  } = useGetAllCodegenFrameworks({
    onSuccess: (frameworks) => {
      if (!frameworks.length) {
        return;
      }

      if (
        !selectedFramework ||
        frameworks.every((fw) => fw.name !== selectedFramework)
      ) {
        selectFramework(frameworks[0].name);
      }
    },
  });

  useEffect(() => {
    getSdlComplete(metadata.actions, metadata.custom_types).then((value) => {
      setActionsSdl(value);
    });
  }, [metadata]);

  const toggleDerivation = () => {
    setShouldDerive((prev) => !prev);
  };

  if (isFetching) {
    return <SkeletonList count={5} />;
  }

  if (error || !allFrameworks?.length) {
    return (
      <IndicatorCard status="negative" showIcon>
        Error fetching codegen assets.&nbsp;
        <Link onClick={() => refetch()} className={'cursor-pointer'}>
          Try again
        </Link>
      </IndicatorCard>
    );
  }

  const getFrameworkActions = () => {
    const getGlitchButton = () => {
      if (selectedFramework !== 'nodejs-express') return null;
      return (
        <Link
          href={getGlitchProjectURL()}
          target="_blank"
          rel="noopener noreferrer"
          className={'mr-5'}
        >
          <BsLink /> Try on glitch
        </Link>
      );
    };

    const getStarterKitButton = () => {
      const selectedFrameworkMetadata = allFrameworks.find(
        (f) => f.name === selectedFramework,
      );
      if (
        selectedFrameworkMetadata &&
        !selectedFrameworkMetadata.hasStarterKit
      ) {
        return null;
      }

      return (
        <Flex align="center" gap="4">
          <Link
            href={getStarterKitDownloadPath(selectedFramework)}
            target="_blank"
            rel="noopener noreferrer"
            title={`Download starter kit for ${selectedFramework}`}
          >
            <Button variant="ghost" leftIcon={BsDownload}>
              <Strong>Starter-kit.zip</Strong>
            </Button>
          </Link>
          <Link
            href={getStarterKitPath(selectedFramework)}
            target="_blank"
            rel="noopener noreferrer"
            title={`View the starter kit for ${selectedFramework} on GitHub`}
          >
            <Button variant="ghost" leftIcon={BsGithub}>
              <Strong>View on GitHub</Strong>
            </Button>
          </Link>
        </Flex>
      );
    };

    const getHelperToolsSection = () => {
      const glitchButton = getGlitchButton();
      const starterKitButtons = getStarterKitButton();
      if (!glitchButton && !starterKitButtons) {
        return null;
      }
      return (
        <div className="ml-auto">
          <div className="text-right mb-1.5">
            <Text weight="bold">Need help getting started quickly?</Text>
          </div>
          <Flex align="center">
            {getGlitchButton()}
            {getStarterKitButton()}
          </Flex>
        </div>
      );
    };

    return (
      <Flex className="mb-5" align="center">
        <Select
          value={selectedFramework}
          onChange={(value) => selectFramework(value)}
          placeholder="Select framework"
          options={allFrameworks.map((f) => ({
            label: f.name,
            value: f.name,
          }))}
        />
        {getHelperToolsSection()}
      </Flex>
    );
  };

  return (
    <Analytics name="ActionsCodegen" {...REDACT_EVERYTHING}>
      {getFrameworkActions()}
      <div>
        <CodeTabs
          framework={selectedFramework}
          actionsSdl={actionsSdl}
          currentAction={currentAction}
          shouldDerive={shouldDerive}
          parentMutation={parentMutation}
        />
      </div>
      <DerivedFrom
        parentMutation={parentMutation}
        shouldDerive={shouldDerive}
        toggleDerivation={toggleDerivation}
      />
    </Analytics>
  );
};

export default Codegen;
