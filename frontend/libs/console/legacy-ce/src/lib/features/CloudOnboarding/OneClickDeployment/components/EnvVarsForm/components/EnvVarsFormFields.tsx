import React from 'react';
import { InputField } from '../../../../../../new-components/Form';
import { Collapsible } from '../../../../../../new-components/Collapsible';
import { RequiredEnvVar } from '../../../types';
import { NeonIcon } from './PgDatabaseField';
import { getEnvVarFormSegments } from '../utils';
import { DatabaseField } from './DatabaseField';

export type EnvVarsFormFieldsProps = {
  envVars: RequiredEnvVar[];
};

export function EnvVarsFormFields(props: EnvVarsFormFieldsProps) {
  const { envVars } = props;

  const {
    isPGDatabaseEnvVarPresent,
    databaseEnvVars,
    dynamicEnvVars,
    staticEnvVars,
  } = React.useMemo(() => getEnvVarFormSegments(envVars), [envVars]);

  return (
    <>
      {databaseEnvVars.length > 0 ? (
        <Collapsible
          defaultOpen
          triggerChildren={
            <div className="flex w-full">
              <span className="font-bold capitalize text-gray-600">
                Database Connections
              </span>
              {isPGDatabaseEnvVarPresent && (
                <>
                  <div className="flex w-[325px]" />
                  <a
                    href="https://neon.tech/"
                    onClick={e => {
                      e.stopPropagation();
                    }}
                    rel="noreferrer noopener"
                    target="_blank"
                  >
                    <div className="flex text-gray-600">
                      Database creation powered by{' '}
                      <div className="ml-2">
                        <NeonIcon />
                      </div>
                    </div>
                  </a>
                </>
              )}
            </div>
          }
        >
          {databaseEnvVars.map((envVar, index) => (
            <div key={index}>
              <DatabaseField envVar={envVar} />
            </div>
          ))}
        </Collapsible>
      ) : null}

      {dynamicEnvVars.length > 0 ? (
        <Collapsible
          defaultOpen
          triggerChildren={
            <span className="font-bold capitalize text-gray-600">
              Variables
            </span>
          }
        >
          {dynamicEnvVars.map((envVar, index) => (
            <div key={index}>
              <InputField
                name={envVar.Name}
                label={envVar.Mandatory ? `${envVar.Name} *` : envVar.Name}
                placeholder={envVar.Name}
                description={envVar.Description}
              />
            </div>
          ))}
        </Collapsible>
      ) : null}

      {staticEnvVars.length > 0 ? (
        <Collapsible
          triggerChildren={
            <span className="font-bold capitalize text-gray-600">
              Preset Variables
            </span>
          }
        >
          {staticEnvVars.map((envVar, index) => (
            <div key={index}>
              <InputField
                name={envVar.Name}
                label={envVar.Mandatory ? `${envVar.Name} *` : envVar.Name}
                placeholder={envVar.Name}
                description={envVar.Description}
                disabled
              />
            </div>
          ))}
        </Collapsible>
      ) : null}
    </>
  );
}
