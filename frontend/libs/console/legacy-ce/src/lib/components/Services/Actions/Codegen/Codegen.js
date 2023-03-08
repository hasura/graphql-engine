import React from 'react';
import Helmet from 'react-helmet';
import {
  Analytics,
  REDACT_EVERYTHING,
  useGetAnalyticsAttributes,
} from '../../../../features/Analytics';
import { getSdlComplete } from '../../../../shared/utils/sdlUtils';
import {
  getAllCodegenFrameworks,
  getStarterKitPath,
  getStarterKitDownloadPath,
  getGlitchProjectURL,
} from './utils';
import Spinner from '../../../Common/Spinner/Spinner';
import { Icon } from '../../../UIKit/atoms';
import CodeTabs from './CodeTabs';
import DerivedFrom from './DerivedFrom';
import { getPersistedDerivedAction } from '../utils';
import { inputStyles } from '../constants';

const Codegen = ({ dispatch, allActions, allTypes, currentAction }) => {
  const [allFrameworks, setAllFrameworks] = React.useState([]);
  const [selectedFramework, selectFramework] = React.useState('');
  const [loading, setLoading] = React.useState(true);
  const [error, setError] = React.useState(null);

  const [parentMutation] = React.useState(
    getPersistedDerivedAction(currentAction.name)
  );
  const [shouldDerive, setShouldDerive] = React.useState(true);

  const toggleDerivation = e => {
    e.preventDefault();
    setShouldDerive(!shouldDerive);
  };

  const init = () => {
    setLoading(true);
    setError(null);
    getAllCodegenFrameworks(dispatch)
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

  const titleAnalyticsAttributes = useGetAnalyticsAttributes('Codegen', {
    redactText: true,
  });

  if (loading) {
    return <Spinner />;
  }

  if (error || !allFrameworks.length) {
    return (
      <div>
        Error fetching codegen assets.&nbsp;
        <a onClick={init} className={'cursor-pointer'}>
          Try again
        </a>
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
          className={`${inputStyles} w-auto`}
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
        <a
          href={getGlitchProjectURL()}
          target="_blank"
          rel="noopener noreferrer"
          className={'mr-5'}
        >
          <Icon type="link" /> Try on glitch
        </a>
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
          <a
            href={getStarterKitDownloadPath(selectedFramework)}
            target="_blank"
            rel="noopener noreferrer"
            className={'mr-5'}
            title={`Download starter kit for ${selectedFramework}`}
          >
            <Icon type="download" /> Starter-kit.zip
          </a>
          <a
            href={getStarterKitPath(selectedFramework)}
            target="_blank"
            rel="noopener noreferrer"
            title={`View the starter kit for ${selectedFramework} on GitHub`}
          >
            <Icon type="github" className={'mr-1'} /> View on GitHub
          </a>
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
        <div className="ml-auto">
          <div className="mb-25 text-right mb-1.5">
            <b>Need help getting started quickly?</b>
          </div>
          <div className="flex">
            {getGlitchButton()}
            {getStarterKitButton()}
          </div>
        </div>
      );
    };

    return (
      <div className="mb-5 flex">
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
    <Analytics name="ActionsCodegen" {...REDACT_EVERYTHING}>
      <div className="w-[600px]">
        <Helmet>
          <title
            {...titleAnalyticsAttributes}
          >{`Codegen - ${currentAction.name} - Actions | Hasura`}</title>
        </Helmet>

        {getFrameworkActions()}
        <div className="mb-5">
          <CodeTabs
            framework={selectedFramework}
            actionsSdl={getSdlComplete(allActions, allTypes)}
            currentAction={currentAction}
            shouldDerive={shouldDerive}
            parentMutation={parentMutation}
            dispatch={dispatch}
          />
        </div>
        <hr className="my-5" />
        {getDerivationInfo()}
      </div>
    </Analytics>
  );
};

export default Codegen;
