import React from 'react';
import { getSdlComplete } from '../../../../shared/utils/sdlUtils';
import {
  getAllCodegenFrameworks,
  getStarterKitPath,
  getGlitchProjectURL,
} from './utils';
import Spinner from '../../../Common/Spinner/Spinner';
import styles from '../Common/components/Styles.scss';
import Button from '../../../Common/Button/Button';
import CodeTabs from './CodeTabs';

const Codegen = ({ allActions, allTypes, currentAction }) => {
  const [allFrameworks, setAllFrameworks] = React.useState([]);
  const [selectedFramework, selectFramework] = React.useState('');
  const [loading, setLoading] = React.useState(true);
  const [error, setError] = React.useState(null);

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
            className={`${styles.default_button} ${styles.add_mar_right_mid}`}
          >
            Try on glitch
          </Button>
        </a>
      );
    };

    const getStarterKitButton = () => {
      return (
        <a
          href={getStarterKitPath(selectedFramework)}
          target="_blank"
          rel="noopener noreferrer"
        >
          <Button color="white" className={`${styles.add_mar_right_mid}`}>
            Get boilerplate
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

  return (
    <div>
      {getFrameworkActions()}
      <CodeTabs
        framework={selectedFramework}
        actionsSdl={getSdlComplete(allActions, allTypes)}
        currentAction={currentAction}
      />
    </div>
  );
};

export default Codegen;
