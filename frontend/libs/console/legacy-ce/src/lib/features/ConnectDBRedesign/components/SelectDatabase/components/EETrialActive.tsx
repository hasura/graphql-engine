import {
  CopyableInputField,
  InformationCard,
} from '@/features/ConnectDBRedesign/components/SelectDatabase/components';
import { dbDisplayNames } from '@/features/ConnectDBRedesign/components/SelectDatabase/databases';
import { indefiniteArticle } from '@/features/ConnectDBRedesign/components/SelectDatabase/utils';
import { DatabaseKind } from '@/features/ConnectDBRedesign/types';
import { Button } from '@/new-components/Button';
import { InputField, SimpleForm } from '@/new-components/Form';
import { hasuraToast } from '@/new-components/Toasts';
import React from 'react';
import { FaExternalLinkAlt } from 'react-icons/fa';
import { GrDocker } from 'react-icons/gr';
import { z } from 'zod';

export const EETrialActive: React.VFC<{ selectedDb: DatabaseKind }> = ({
  selectedDb,
}) => {
  const dbWithArticle = `${indefiniteArticle(selectedDb)} ${
    dbDisplayNames[selectedDb]
  }`;
  return (
    <InformationCard blueLeftBorder>
      <div className="flex flex-col">
        <div className="flex items-center pb-3 mb-3 border-b border-slate-300">
          <div className="flex flex-col w-3/4">
            <div className="font-bold">
              {dbDisplayNames[selectedDb]} Connector Required
            </div>
            <div className="text-md text-gray-700">
              {`The Hasura GraphQL Data Connector Service is required to connect to ${dbWithArticle} database.`}
            </div>
          </div>
          <div className="flex w-1/4 justify-end">
            <Button
              icon={<FaExternalLinkAlt />}
              iconPosition="end"
              onClick={() => {
                alert('need link to docs here');
              }}
            >
              Deployment Methods
            </Button>
          </div>
        </div>
        <div>
          <div className="font-bold text-muted flex items-center">
            <GrDocker />
            <div className="ml-1">Docker Initialization</div>
          </div>
          <SimpleForm
            schema={z.object({
              docker_command: z.string(),
              agent_path: z.string(),
            })}
            options={{
              defaultValues: {
                docker_command:
                  'docker run -p 127.0.0.1:1234:1234 hasura/graphql-data-connector',
                agent_path: 'http://host.docker.internal:1234',
              },
            }}
            onSubmit={values => {
              console.log(values);
            }}
          >
            <CopyableInputField
              className="mt-1"
              label="Run GraphQL Data Connector Service"
              tooltip="This is a really great tooltip for this field"
              disabled={true}
              name="docker_command"
              learnMoreLink="https://hasura.io/docs"
            />
            <InputField
              name="agent_path"
              label="Connect to GraphQL Data Connector URI"
              tooltip="This is a really great tooltip for this field"
            />
            <div className="flex justify-end w-full">
              <Button
                type="submit"
                mode="primary"
                onClick={() => {
                  hasuraToast({
                    title: 'Not Implemented',
                    message:
                      'This feature will be implemented once the dc_add_agent check is merged.',
                    toastOptions: {
                      duration: 3000,
                    },
                  });
                }}
              >
                Validate And Connect Database
              </Button>
            </div>
          </SimpleForm>
        </div>
      </div>
    </InformationCard>
  );
};
