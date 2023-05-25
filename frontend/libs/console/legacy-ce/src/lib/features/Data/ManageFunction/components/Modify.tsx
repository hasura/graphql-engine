import { useState } from 'react';
import { Button } from '../../../../new-components/Button';
import { QualifiedFunction } from '../../../hasura-metadata-types';
import { ModifyFunctionConfiguration } from './ModifyFunctionConfiguration';
import { IconTooltip } from '../../../../new-components/Tooltip';
import { FaEdit, FaQuestionCircle } from 'react-icons/fa';
import { DisplayConfigurationDetails } from './DisplayConfigurationDetails';

export type ModifyProps = {
  dataSourceName: string;
  qualifiedFunction: QualifiedFunction;
};

export const Modify = (props: ModifyProps) => {
  const [isEditConfigurationModalOpen, setIsEditConfigurationModalOpen] =
    useState(false);

  return (
    <div className="py-4">
      <div className="w-full bg-white p-4 rounded-sm border my-2">
        <div className="flex gap-2 mb-sm items-center">
          <div className="font-semibold text-2xl">Configuration</div>
          <IconTooltip
            message="allows you to customize any given function with a custom name and custom root fields of an already tracked function. This will replace the already present customization."
            icon={<FaQuestionCircle />}
          />
        </div>

        <DisplayConfigurationDetails {...props} />
        <div className="flex justify-end">
          <Button
            onClick={() => setIsEditConfigurationModalOpen(true)}
            icon={<FaEdit />}
          >
            Edit Configuration
          </Button>
        </div>
        {isEditConfigurationModalOpen && (
          <ModifyFunctionConfiguration
            {...props}
            onSuccess={() => setIsEditConfigurationModalOpen(false)}
            onClose={() => setIsEditConfigurationModalOpen(false)}
          />
        )}
      </div>
    </div>
  );
};
