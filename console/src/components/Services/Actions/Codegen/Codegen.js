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
import { Spinner, TextLink } from '../../../UIKit/atoms';
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
        <TextLink onClick={init}>Try again</TextLink>
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
        <TextLink
          href={getGlitchProjectURL()}
          target="_blank"
          mr="20px"
          hover="underline"
        >
          <Icon type="link" mb="-2px" mr="3px" /> Try on glitch
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
        <React.Fragment>
          <TextLink
            href={getStarterKitDownloadPath(selectedFramework)}
            target="_blank"
            mr="20px"
            title={`Download starter kit for ${selectedFramework}`}
            hover="underline"
          >
            <Icon type="download" mr="3px" /> Starter-kit.zip
          </TextLink>
          <TextLink
            href={getStarterKitPath(selectedFramework)}
            target="_blank"
            title={`View the starter kit for ${selectedFramework} on GitHub`}
            hover="underline"
          >
            <Icon type="github" mr="3px" mb="-2px" /> View on GitHub
          </TextLink>
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
        <div className={styles.marginLeftAuto}>
          <div
            className={`${styles.add_mar_bottom_small} ${styles.textAlignRight}`}
          >
            <b>Need help getting started quickly?</b>
          </div>
          <div className={`${styles.display_flex}`}>
            {getGlitchButton()}
            {getStarterKitButton()}
          </div>
        </div>
      );
    };

    return (
      <div className={`${styles.add_mar_bottom} ${styles.display_flex}`}>
        {getDrodown()}
        {getHelperToolsSection()}
      </div>
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
    <div style={{ width: '600px' }}>
      <Helmet
        title={`Codegen - ${currentAction.action_name} - Actions | Hasura`}
      />
      {getFrameworkActions()}
      <div className={`${styles.add_mar_bottom}`}>
        <CodeTabs
          framework={selectedFramework}
          actionsSdl={getSdlComplete(allActions, allTypes)}
          currentAction={currentAction}
          shouldDerive={shouldDerive}
          parentMutation={parentMutation}
        />
      </div>
      <hr />
      {getDerivationInfo()}
    </div>
  );
};

export default Codegen;
