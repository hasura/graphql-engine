import React, { useState, useEffect } from 'react';
import { useDebouncedEffect } from '@hasura/shared/hooks';
import { TransformationType } from './stateDefaults';
import KeyValueInput from './CustomEditors/KeyValueInput';
import NumberedSidebar from './CustomEditors/NumberedSidebar';
import { editorDebounceTime, setEnvVarsToLS } from './utils';
import { NameValue } from '@hasura/shared/types';
import { Text } from '@hasura/shared/ui';
import { Grid } from '@radix-ui/themes';

type SampleContextTransformsProps = {
  transformationType: TransformationType;
  envVars: NameValue[];
  sessionVars: NameValue[];
  envVarsOnChange: (envVars: NameValue[]) => void;
  sessionVarsOnChange: (sessionVars: NameValue[]) => void;
};

const SampleContextTransforms: React.FC<SampleContextTransformsProps> = ({
  transformationType,
  envVars,
  sessionVars,
  envVarsOnChange,
  sessionVarsOnChange,
}) => {
  const [localEnvVars, setLocalEnvVars] = useState<NameValue[]>(envVars);
  const [localSessionVars, setLocalSessionVars] =
    useState<NameValue[]>(sessionVars);

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
    [localEnvVars],
  );

  useDebouncedEffect(
    () => {
      sessionVarsOnChange(localSessionVars);
    },
    editorDebounceTime,
    [localSessionVars],
  );

  return (
    <div className="ml-4 pl-10 pt-4 mb-4 border-l border-l-(--gray-a7)">
      <div className="mb-4">
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
        <Grid columns="3" gap="3" className="mb-2">
          <div>
            <Text weight="medium">Env Variables</Text>
          </div>
          <div>
            <Text weight="medium">Value</Text>
          </div>
        </Grid>
        <KeyValueInput
          pairs={localEnvVars}
          setPairs={(ev) => {
            setLocalEnvVars(ev);
          }}
          testId="env-vars"
        />
      </div>

      {transformationType !== 'event' && (
        <div className="mb-4">
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
          <Grid columns="3" gap="3" className="mb-2">
            <div>
              <Text weight="medium">Session Variables</Text>
            </div>
            <div>
              <Text weight="medium">Value</Text>
            </div>
          </Grid>
          <KeyValueInput
            pairs={localSessionVars}
            setPairs={(sv) => {
              setLocalSessionVars(sv);
            }}
            testId="session-vars"
          />
        </div>
      )}
    </div>
  );
};

export default SampleContextTransforms;
