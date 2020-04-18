import React from 'react';
import Helmet from 'react-helmet';

import { getSdlComplete } from '../../../../shared/utils/sdlUtils';
import {
  getAllCodegenFrameworks,
  getStarterKitPath,
  getGlitchProjectURL,
} from './utils';
import { getPersistedDerivedAction } from '../lsUtils';
import Button from '../../../Common/Button/Button';
import CodeTabs from './CodeTabs';
import DerivedFrom from './DerivedFrom';
import { Spinner, TextLink, Flex, Box } from '../../../UIKit/atoms';
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
        <TextLink onClick={init} hover="underline">
          Try again
        </TextLink>
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
        <TextLink href={getGlitchProjectURL()} target="_blank">
          <Button
            color="white"
            className={`${styles.add_mar_right_mid} ${styles.default_button}`}
          >
            Try on glitch
          </Button>
        </TextLink>
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
        <TextLink href={getStarterKitPath(selectedFramework)} target="_blank">
          <Button color="white" className={`${styles.add_mar_right_mid}`}>
            Get starter kit
          </Button>
        </TextLink>
      );
    };

    return (
      <Flex mb="20px">
        {getDrodown()}
        {getGlitchButton()}
        {getStarterKitButton()}
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
