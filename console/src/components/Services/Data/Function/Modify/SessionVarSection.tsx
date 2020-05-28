import React, { useState } from 'react';
import styles from './ModifyCustomFunction.scss';
import ToolTip from '../../../../Common/Tooltip/Tooltip';
import Button from '../../../../Common/Button';
import EditorInput from './EditorInput';

type ConfigurationType = {
  session_argument: string;
};

export interface SessionVarSectionProps {
  functionName?: string;
  onSessVarUpdate: (s: string) => void;
  configuration?: ConfigurationType;
}
const SessionVarSection: React.FC<SessionVarSectionProps> = ({
  onSessVarUpdate,
  configuration,
  functionName,
}) => {
  const [sessVar, setSessVar] = useState('');
  const [isEditing, setIsEditing] = useState(false);
  const toggleIsEditting = () => setIsEditing(b => !b);

  const onSessVarChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setSessVar(e.target.value);
  };
  const onSave = () => {
    if (sessVar) onSessVarUpdate(sessVar);
  };

  return (
    <>
      <hr />
      <h4 className={styles.subheading_text}>
        Session Variable Argument
        <ToolTip message="Use Session Variable argument as SQL function input argument." />
      </h4>
      <div
        className={`${
          isEditing ? styles.editorExpanded : styles.editorCollapsed
        }`}
      >
        <div
          className={`${styles.display_flex} ${styles.add_mar_bottom_mid}`}
          data-test={`${functionName}-session-argument`}
        >
          <Button
            className={`${styles.add_mar_small}`}
            color="white"
            size="xs"
            onClick={toggleIsEditting}
            data-test={`${functionName}-session-argument-btn`}
          >
            {isEditing ? 'Close' : 'Edit'}
          </Button>
          {configuration?.session_argument || 'No Session Argument selected'}
        </div>
        {isEditing && (
          <>
            <EditorInput
              label="Session Variable Argument"
              value={sessVar}
              onChange={onSessVarChange}
              testID={functionName}
              placeholder="hasura-session"
            />
            <div className={styles.add_mar_top_small}>
              <Button
                type="submit"
                color="yellow"
                size="sm"
                className={styles.add_mar_right}
                onClick={onSave}
                data-test={`${functionName}-session-argument-save`}
                disabled={!sessVar.length}
              >
                Save
              </Button>
            </div>
          </>
        )}
      </div>
    </>
  );
};

export default SessionVarSection;
