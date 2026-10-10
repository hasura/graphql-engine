import { useState } from 'react';
import { Flex } from '@radix-ui/themes';
import { Button, RawSqlButton, SqlCodeBlock, Text } from '@hasura/shared/ui';
import { MetadataFunction, Source } from '@hasura/shared/types';
import { ModifyFunctionConfiguration } from './ModifyFunctionConfiguration';
import { FaEdit } from 'react-icons/fa';
import { DisplayConfigurationDetails } from './DisplayConfigurationDetails';
import { GetFunctionDefinitionResult } from '@hasura/metadata/data-source';
import FunctionCommentEditor from './FunctionCommentEditor';
import { useAppContext } from '@hasura/shared/context';

export type ModifyProps = {
  source: Source;
  currentFunction: MetadataFunction;
  functionDefinition: GetFunctionDefinitionResult | null | undefined;
  refetchFunctionDefinition?: () => void;
};

export const Modify = ({
  source,
  currentFunction,
  functionDefinition,
}: ModifyProps) => {
  const { readOnlyMode } = useAppContext();
  const [isEditConfigurationModalOpen, setIsEditConfigurationModalOpen] =
    useState(false);

  return (
    <Flex className="py-4" direction="column" gap="4">
      <Flex gap="2" align="center">
        {!readOnlyMode && (
          <Button
            mode="default"
            size="sm"
            onClick={() => setIsEditConfigurationModalOpen(true)}
            leftIcon={FaEdit}
          >
            Edit Configuration
          </Button>
        )}
      </Flex>
      <DisplayConfigurationDetails
        source={source}
        currentFunction={currentFunction}
        functionDefinition={functionDefinition}
      />
      {isEditConfigurationModalOpen && (
        <ModifyFunctionConfiguration
          currentFunction={currentFunction}
          source={source}
          isVolatile={functionDefinition?.isVolatile}
          onSuccess={() => setIsEditConfigurationModalOpen(false)}
          onClose={() => setIsEditConfigurationModalOpen(false)}
        />
      )}
      <div className="w-full md:w-8/12">
        <FunctionCommentEditor
          source={source}
          func={currentFunction}
          defaultValue={currentFunction.configuration?.comment ?? ''}
          readOnly={readOnlyMode}
        />
      </div>
      {functionDefinition?.definition ? (
        <div className="w-full md:w-8/12">
          <Flex align="center" gap="2" className="mb-2">
            <Text weight="medium">Function Definition:</Text>
            {!readOnlyMode ? (
              <RawSqlButton
                sql={functionDefinition.definition}
                data-test="modify-view"
              >
                Modify
              </RawSqlButton>
            ) : null}
          </Flex>

          <div>
            <SqlCodeBlock
              language={source.kind}
              text={functionDefinition.definition}
            />
          </div>
        </div>
      ) : null}
    </Flex>
  );
};
