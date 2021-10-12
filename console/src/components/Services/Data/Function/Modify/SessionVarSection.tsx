import React, { useState, useEffect } from 'react';
import styles from './ModifyCustomFunction.scss';
import ToolTip from '../../../../Common/Tooltip/Tooltip';
import Button from '../../../../Common/Button';
import EditorInput from './EditorInput';
import KnowMoreLink from '../../../../Common/KnowMoreLink/KnowMoreLink';

type ConfigurationType = {
  session_argument: string;
};
export type FunctionLoadingType = {
  isRequesting?: boolean;
  isDeleting?: boolean;
  isUntracking?: boolean;
  isFetching?: boolean;
} | null;
export interface SessionVarSectionProps {
  functionName?: string;
  onSessVarUpdate: (s: string) => Promise<void>;
  configuration?: ConfigurationType;
  loading: FunctionLoadingType;
}
const SessionVarSection: React.FC<SessionVarSectionProps> = ({
  onSessVarUpdate,
  configuration,
  functionName,
}) => {
  const [sessVar, setSessVar] = useState(configuration?.session_argument || '');
  const [isEditing, setIsEditing] = useState(false);
  const toggleIsEditting = () => setIsEditing(prev => !prev);

  useEffect(() => {
    setSessVar(prev => prev || configuration?.session_argument || '');
  }, [configuration]);

  const onSessVarChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setSessVar(e.target.value);
  };
  const closeEditPopUp = React.useCallback(() => setIsEditing(false), [
    setIsEditing,
  ]);
  const onSave = () => {
    if (sessVar !== (configuration?.session_argument || ''))
      onSessVarUpdate(sessVar).then(closeEditPopUp);
  };

  return (
    <>
      <hr className="my-md" />
      <h4 className={styles.subheading_text}>
        Session argument
        <ToolTip message="the function argument into which hasura session variables will be passed" />
        <KnowMoreLink href="https://hasura.io/docs/latest/graphql/core/schema/custom-functions.html#accessing-hasura-session-variables-in-custom-functions" />
      </h4>
      <div
        className={isEditing ? styles.editorExpanded : styles.editorCollapsed}
      >
        <div
          className={`${styles.display_flex} ${styles.add_mar_bottom_mid}`}
          data-test={`${functionName}-session-argument`}
        >
          <Button
            className={styles.add_mar_small}
            color="white"
            size="xs"
            onClick={toggleIsEditting}
            data-test={`${functionName}-session-argument-btn`}
          >
            {isEditing ? 'Close' : 'Edit'}
          </Button>
          {configuration?.session_argument || 'No Session argument'}
        </div>
        {isEditing && (
          <>
            <EditorInput
              label="Session argument"
              value={sessVar}
              onChange={onSessVarChange}
              testID={functionName}
              placeholder="hasura_session"
            />
            <div className={styles.add_mar_top_small}>
              <Button
                type="submit"
                color="yellow"
                size="sm"
                className={styles.add_mar_right}
                onClick={onSave}
                data-test={`${functionName}-session-argument-save`}
                disabled={sessVar === configuration?.session_argument}
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
