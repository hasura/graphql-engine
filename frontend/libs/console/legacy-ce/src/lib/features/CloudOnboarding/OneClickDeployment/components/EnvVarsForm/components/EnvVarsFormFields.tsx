import React from 'react';
import { Flex, Link } from '@radix-ui/themes';
import { InputField, Collapsible, Text } from '@hasura/shared/ui';
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
            <Flex className="w-full" align="center" gap="2">
              <Text weight="bold" className="capitalize">
                Database Connections
              </Text>
              {isPGDatabaseEnvVarPresent && (
                <>
                  <Flex className="w-[325px]" />
                  <Link
                    href="https://neon.tech/"
                    onClick={(e) => {
                      e.stopPropagation();
                    }}
                    rel="noreferrer noopener"
                    target="_blank"
                    color="gray"
                  >
                    <Flex align="center" gap="2">
                      <Text>Database creation powered by</Text>
                      <NeonIcon />
                    </Flex>
                  </Link>
                </>
              )}
            </Flex>
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
            <Text weight="bold" className="capitalize">
              Variables
            </Text>
          }
        >
          {dynamicEnvVars.map((envVar, index) => (
            <div key={index}>
              <InputField
                name={envVar.Name}
                label={envVar.Mandatory ? `${envVar.Name} *` : envVar.Name}
                description={envVar.Description}
                fieldProps={{
                  placeholder: envVar.Name,
                }}
                noErrorPlaceholder
              />
            </div>
          ))}
        </Collapsible>
      ) : null}

      {staticEnvVars.length > 0 ? (
        <Collapsible
          triggerChildren={
            <Text weight="bold" className="capitalize">
              Preset Variables
            </Text>
          }
        >
          {staticEnvVars.map((envVar, index) => (
            <div key={index}>
              <InputField
                name={envVar.Name}
                label={envVar.Mandatory ? `${envVar.Name} *` : envVar.Name}
                description={envVar.Description}
                noErrorPlaceholder
                fieldProps={{
                  placeholder: envVar.Name,
                  disabled: true,
                }}
              />
            </div>
          ))}
        </Collapsible>
      ) : null}
    </>
  );
}
