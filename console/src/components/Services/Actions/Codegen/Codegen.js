import React from 'react';
import Helmet from 'react-helmet';
import { getSdlComplete } from '../../../../shared/utils/sdlUtils';
import {
  getAllCodegenFrameworks,
  getStarterKitPath,
  getGlitchProjectURL,
} from './utils';
import { getPersistedDerivedAction } from '../lsUtils';
import Spinner from '../../../Common/Spinner/Spinner';
import styles from '../Common/components/Styles.scss';
import Button from '../../../Common/Button/Button';
import CodeTabs from './CodeTabs';
import DerivedFrom from './DerivedFrom';

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
    return <Spinner />;
  }

  if (error || !allFrameworks.length) {
    return (
      <div>
        Error fetching codegen assets.&nbsp;
        <a onClick={init}>Try again</a>
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
        <a
          href={getGlitchProjectURL()}
          target="_blank"
          rel="noopener noreferrer"
        >
          <Button
            color="white"
            className={`${styles.add_mar_right_mid} ${styles.default_button}`}
          >
            Try on glitch
          </Button>
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
        <a
          href={getStarterKitPath(selectedFramework)}
          target="_blank"
          rel="noopener noreferrer"
        >
          <Button color="white" className={`${styles.add_mar_right_mid}`}>
            Get starter kit
          </Button>
        </a>
      );
    };

    return (
      <div className={`${styles.add_mar_bottom} ${styles.display_flex}`}>
        {getDrodown()}
        {getGlitchButton()}
        {getStarterKitButton()}
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
