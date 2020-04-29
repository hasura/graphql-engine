import React from 'react';
import Helmet from 'react-helmet';

import { getSdlComplete } from '../../../../shared/utils/sdlUtils';
import {
  getAllCodegenFrameworks,
  getStarterKitPath,
  getGlitchProjectURL,
  getStarterKitDownloadPath,
} from './utils';
import { getPersistedDerivedAction } from '../lsUtils';

import { Icon } from '../../../UIKit/atoms';
import CodeTabs from './CodeTabs';
import DerivedFrom from './DerivedFrom';
import { Spinner, Link, Flex, Box, Text } from '../../../UIKit/atoms';
import styles from '../Common/components/Styles.scss';

const Codegen = ({ allActions, allTypes, currentAction }) => {
  const [allFrameworks, setAllFrameworks] = React.useState([]);
  const [selectedFramework, selectFramework] = React.useState('');
  const [loading, setLoading] = React.useState(true);
  const [error, setError] = React.useState(null);

  const [parentMutation] = React.useState(
    getPersistedDerivedAction(currentAction.action_name)
  );
  const [shouldDerive, setShouldDerive] = React.useState(true);

  const toggleDerivation = e => {
    e.preventDefault();
    setShouldDerive(!shouldDerive);
  };

  const init = () => {
    setLoading(true);
    setError(null);
    getAllCodegenFrameworks()
      .then(frameworks => {
        setAllFrameworks(frameworks);
        selectFramework(frameworks[0].name);
        setLoading(false);
      })
      .catch(e => {
        console.error(e);
        setLoading(false);
        setError(e);
      });
  };

  React.useEffect(init, []);

  if (loading) {
    return <Spinner size="xl" my="100px" mx="auto" />;
  }

  if (error || !allFrameworks.length) {
    return (
      <div>
        Error fetching codegen assets.&nbsp;
        <Link onClick={init}>Try again</Link>
      </div>
    );
  }

  const getFrameworkActions = () => {
    const onChange = e => {
      selectFramework(e.target.value);
    };

    const getDrodown = () => {
      return (
        <select
          className={`form-control ${styles.inputWidth} ${styles.add_mar_right} ${styles.add_mar_right}`}
          value={selectedFramework}
          onChange={onChange}
        >
          {allFrameworks.map(f => {
            return (
              <option key={f.name} value={f.name}>
                {f.name}
              </option>
            );
          })}
        </select>
      );
    };

    const getGlitchButton = () => {
      if (selectedFramework !== 'nodejs-express') return null;
      return (
        <Link
          href={getGlitchProjectURL()}
          target="_blank"
          mr="20px"
          hover="underline"
        >
          <Icon type="link" mb="-2px" mr="3px" /> Try on glitch
        </Link>
      );
    };

    const getStarterKitButton = () => {
      const selectedFrameworkMetadata = allFrameworks.find(
        f => f.name === selectedFramework
      );
      if (
        selectedFrameworkMetadata &&
        !selectedFrameworkMetadata.hasStarterKit
      ) {
        return null;
      }

      return (
        <React.Fragment>
          <Link
            href={getStarterKitDownloadPath(selectedFramework)}
            target="_blank"
            mr="20px"
            title={`Download starter kit for ${selectedFramework}`}
            hover="underline"
          >
            <Icon type="download" mr="3px" /> Starter-kit.zip
          </Link>
          <Link
            href={getStarterKitPath(selectedFramework)}
            target="_blank"
            title={`View the starter kit for ${selectedFramework} on GitHub`}
            hover="underline"
          >
            <Icon type="github" mr="3px" mb="-2px" /> View on GitHub
          </Link>
        </React.Fragment>
      );
    };

    const getHelperToolsSection = () => {
      const glitchButton = getGlitchButton();
      const starterKitButtons = getStarterKitButton();

      if (!glitchButton && !starterKitButtons) {
        return null;
      }

      return (
        <Box ml="auto">
          <Text fontWeight="bold" textAlign="right" mb="5px">
            Need help getting started quickly?
          </Text>
          <Flex mb="20px">
            {getGlitchButton()}
            {getStarterKitButton()}
          </Flex>
        </Box>
      );
    };

    return (
      <Flex mb="20px">
        {getDrodown()}
        {getHelperToolsSection()}
      </Flex>
    );
  };

  const getDerivationInfo = () => {
    return (
      <DerivedFrom
        parentMutation={parentMutation}
        shouldDerive={shouldDerive}
        toggleDerivation={toggleDerivation}
      />
    );
  };

  return (
    <Box width="600px">
      <Helmet
        title={`Codegen - ${currentAction.action_name} - Actions | Hasura`}
      />
      {getFrameworkActions()}
      <Box mb="20px">
        <CodeTabs
          framework={selectedFramework}
          actionsSdl={getSdlComplete(allActions, allTypes)}
          currentAction={currentAction}
          shouldDerive={shouldDerive}
          parentMutation={parentMutation}
        />
      </Box>
      <hr />
      {getDerivationInfo()}
    </Box>
  );
};

export default Codegen;
