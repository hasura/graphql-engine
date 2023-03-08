import React, { useState, useEffect } from 'react';
import { IconTooltip } from '../../../../../new-components/Tooltip';
import { Button } from '../../../../../new-components/Button';
import { LearnMoreLink } from '../../../../../new-components/LearnMoreLink';
import styles from './ModifyCustomFunction.module.scss';
import EditorInput from './EditorInput';

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
  const closeEditPopUp = React.useCallback(
    () => setIsEditing(false),
    [setIsEditing]
  );
  const onSave = () => {
    if (sessVar !== (configuration?.session_argument || ''))
      onSessVarUpdate(sessVar).then(closeEditPopUp);
  };

  return (
    <>
      <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
        Session Argument
        <IconTooltip message="the function argument into which hasura session variables will be passed" />
        <LearnMoreLink
          href="https://hasura.io/docs/latest/graphql/core/schema/custom-functions.html#accessing-hasura-session-variables-in-custom-functions"
          className="font-normal"
        />
      </h4>
      <div
        className={
          isEditing
            ? 'space-y-md block rounded bg-white border border-gray-300 p-md mb-sm'
            : 'space-y-md block'
        }
      >
        <div className="mb-md" data-test={`${functionName}-session-argument`}>
          <Button
            className={styles.add_mar_small}
            onClick={toggleIsEditting}
            data-test={`${functionName}-session-argument-btn`}
          >
            {isEditing ? 'Close' : 'Edit'}
          </Button>
        </div>
        {isEditing && (
          <>
            <EditorInput
              label="Session Argument"
              value={sessVar}
              onChange={onSessVarChange}
              testID={functionName}
              placeholder="hasura_session"
            />
            <div className="mt-md">
              <Button
                type="submit"
                mode="primary"
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
