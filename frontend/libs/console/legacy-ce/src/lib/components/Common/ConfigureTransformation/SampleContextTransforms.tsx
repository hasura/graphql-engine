import React, { useState, useEffect } from 'react';
import { useDebouncedEffect } from '../../../hooks/useDebounceEffect';
import { KeyValuePair, TransformationType } from './stateDefaults';
import KeyValueInput from './CustomEditors/KeyValueInput';
import NumberedSidebar from './CustomEditors/NumberedSidebar';
import { editorDebounceTime, setEnvVarsToLS } from './utils';

type SampleContextTransformsProps = {
  transformationType: TransformationType;
  envVars: KeyValuePair[];
  sessionVars: KeyValuePair[];
  envVarsOnChange: (envVars: KeyValuePair[]) => void;
  sessionVarsOnChange: (sessionVars: KeyValuePair[]) => void;
};

const SampleContextTransforms: React.FC<SampleContextTransformsProps> = ({
  transformationType,
  envVars,
  sessionVars,
  envVarsOnChange,
  sessionVarsOnChange,
}) => {
  const [localEnvVars, setLocalEnvVars] = useState<KeyValuePair[]>(envVars);
  const [localSessionVars, setLocalSessionVars] =
    useState<KeyValuePair[]>(sessionVars);

  useEffect(() => {
    setLocalEnvVars(envVars);
  }, [envVars]);

  useEffect(() => {
    setLocalSessionVars(sessionVars);
  }, [sessionVars]);

  useDebouncedEffect(
    () => {
      envVarsOnChange(localEnvVars);
      setEnvVarsToLS(localEnvVars);
    },
    editorDebounceTime,
    [localEnvVars]
  );

  useDebouncedEffect(
    () => {
      sessionVarsOnChange(localSessionVars);
    },
    editorDebounceTime,
    [localSessionVars]
  );

  return (
    <div className="m-md pl-lg pr-sm border-l border-l-gray-400">
      <div className="mb-md">
        <NumberedSidebar
          title="Sample Env Variables"
          description={
            <span>
              Enter a sample input for your provided env variables.
              <br />
              e.g. the sample value for {transformationType.toUpperCase()}
              _BASE_URL
            </span>
          }
          number={transformationType === 'event' ? '' : '1'}
        />
        <div className="grid gap-3 grid-cols-3">
          <div>
            <label className="block text-gray-600 font-medium mb-xs">
              Env Variables
            </label>
          </div>
          <div>
            <label className="block text-gray-600 font-medium mb-xs">
              Value
            </label>
          </div>
        </div>
        <div className="grid gap-3 grid-cols-3 mb-sm">
          <KeyValueInput
            pairs={localEnvVars}
            setPairs={ev => {
              setLocalEnvVars(ev);
            }}
            testId="env-vars"
          />
        </div>
      </div>

      {transformationType !== 'event' && (
        <div className="mb-md">
          <NumberedSidebar
            title="Sample Session Variables"
            description={
              <span>
                Enter a sample input for your provided session variables.
                <br />
                e.g. the sample value for x-hasura-user-id
              </span>
            }
            number="2"
          />
          <div className="grid gap-3 grid-cols-3">
            <div>
              <label className="block text-gray-600 font-medium mb-xs">
                Session Variables
              </label>
            </div>
            <div>
              <label className="block text-gray-600 font-medium mb-xs">
                Value
              </label>
            </div>
          </div>
          <div className="grid gap-3 grid-cols-3 mb-sm">
            <KeyValueInput
              pairs={localSessionVars}
              setPairs={sv => {
                setLocalSessionVars(sv);
              }}
              testId="session-vars"
            />
          </div>
        </div>
      )}
    </div>
  );
};

export default SampleContextTransforms;
