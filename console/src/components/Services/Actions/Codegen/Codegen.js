import React from 'react';
import { getSdlComplete } from '../../../../shared/utils/sdlUtils';
import { getAllCodegenFrameworks } from './utils';
import Spinner from '../../../Common/Spinner/Spinner';
import styles from '../Common/UIComponents/Styles.scss';
import CodeTabs from './CodeTabs';

const Codegen = ({ allActions, allTypes, currentAction }) => {
  const [allFrameworks, setAllFrameworks] = React.useState([]);
  const [selectedFramework, selectFramework] = React.useState('');
  const [loading, setLoading] = React.useState(true);
  const [error, setError] = React.useState(null);

  const init = () => {
    console.log('initializing');
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

  if (error || !allFrameworks.length) {
    return (
      <div>
        Error fetching codegen assets.&nbsp;
        <a onClick={init}>Try again</a>
      </div>
    );
  }

  if (loading) {
    return <Spinner />;
  }

  const getFrameworkDropDown = () => {
    const onChange = e => {
      selectFramework(e.target.value);
    };
    return (
      <select
        className={`form-control ${styles.inputWidth} ${styles.add_mar_right} ${
          styles.add_mar_bottom
        }`}
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

  return (
    <div>
      {getFrameworkDropDown()}
      <CodeTabs
        framework={selectedFramework}
        actionsSdl={getSdlComplete(allActions, allTypes)}
        currentAction={currentAction}
      />
    </div>
  );
};

export default Codegen;
