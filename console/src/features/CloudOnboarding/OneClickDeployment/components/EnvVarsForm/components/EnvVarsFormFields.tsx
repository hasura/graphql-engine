import React from 'react';
import { InputField } from '@/new-components/Form';
import { Collapsible } from '@/new-components/Collapsible';
import { RequiredEnvVar } from '../../../types';
import { postgresSubKind } from '../../../constants';
import { PgDatabaseField } from './PgDatabaseField';

export type EnvVarsFormFieldsProps = {
  envVars: RequiredEnvVar[];
};

export function EnvVarsFormFields(props: EnvVarsFormFieldsProps) {
  const { envVars } = props;

  const databaseEnvVars = envVars.filter(ev => ev.Kind === 'ENV_TYPE_DATABASE');

  const dynamicEnvVars = envVars.filter(ev => ev.Kind === 'ENV_TYPE_DYNAMIC');

  return (
    <>
      {databaseEnvVars.length > 0 ? (
        <Collapsible
          defaultOpen
          triggerChildren={
            <span className="font-semibold capitalize text-gray-600">
              Databases
            </span>
          }
        >
          {databaseEnvVars.map((envVar, index) => (
            <div key={index}>
              {envVar.SubKind === postgresSubKind ? (
                <PgDatabaseField dbEnvVar={envVar} />
              ) : (
                <InputField
                  name={envVar.Name}
                  label={`${envVar.Name} *`}
                  placeholder={envVar.Name}
                  description={envVar.Description}
                />
              )}
            </div>
          ))}
        </Collapsible>
      ) : null}

      {dynamicEnvVars.length > 0 ? (
        <Collapsible
          defaultOpen
          triggerChildren={
            <span className="font-semibold capitalize text-gray-600">
              Environment Variables
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
    </>
  );
}
