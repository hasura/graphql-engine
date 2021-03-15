import React from 'react';
import Spinner from '../../../Common/Spinner/Spinner';
import AceEditor from '../../../Common/AceEditor/BaseEditor';
import { getLanguageModeFromExtension } from '../../../Common/AceEditor/utils';
import { getFrameworkCodegen } from './utils';
import { getFileExtensionFromFilename } from '../../../Common/utils/jsUtils';
import { Tabs, Tab } from 'react-bootstrap';
import styles from '../Actions.scss';

const CodeTabs = ({
  framework,
  actionsSdl,
  currentAction,
  parentMutation,
  shouldDerive,
  dispatch,
}) => {
  const [loading, setLoading] = React.useState(true);
  const [error, setError] = React.useState(null);
  const [codegenFiles, setCodegenFiles] = React.useState([]);

  const init = () => {
    setLoading(true);
    setError(null);
    getFrameworkCodegen(
      framework,
      currentAction.name,
      actionsSdl,
      shouldDerive ? parentMutation : null,
      dispatch
    )
      .then(codeFiles => {
        setCodegenFiles(codeFiles);
        setLoading(false);
      })
      .catch(e => {
        setError(e);
        setLoading(false);
      });
  };

  React.useEffect(init, [framework, parentMutation, shouldDerive]);

  if (loading) {
    return <Spinner />;
  }

  if (error) {
    return (
      <div>
        Error generating code.&nbsp;
        <a onClick={init} className={styles.cursorPointer}>
          Try again
        </a>
      </div>
    );
  }

  const files = codegenFiles.map(({ name, content }) => {
    const editorProps = {
      width: '600px',
      value: content.trim(),
      readOnly: true,
      mode: getLanguageModeFromExtension(getFileExtensionFromFilename(name)),
    };
    return (
      <Tab eventKey={name} title={name} key={name}>
        <AceEditor {...editorProps} />
      </Tab>
    );
  });

  return <Tabs id="codegen-files-tabs">{files} </Tabs>;
};

export default CodeTabs;
