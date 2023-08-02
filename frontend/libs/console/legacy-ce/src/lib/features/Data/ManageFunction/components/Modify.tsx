import { useState } from 'react';
import { Button } from '../../../../new-components/Button';
import {
  QualifiedFunction,
  SupportedDrivers,
} from '../../../hasura-metadata-types';
import { ModifyFunctionConfiguration } from './ModifyFunctionConfiguration';
import { ModifyFunctionPermissionsDialog } from '../../../Permissions/FunctionPermissions/ModifyFunctionPermissionsDialog';
import { IconTooltip } from '../../../../new-components/Tooltip';
import { FaEdit, FaKey } from 'react-icons/fa';
import { DisplayConfigurationDetails } from './DisplayConfigurationDetails';
import { FunctionGraphQLCustomization } from '../../../../components/Services/Data/Function/Modify/GraphQLCustomization/FunctionGraphQLCustomization';
import { useMetadata } from '../../../hasura-metadata-api';
import { findSource } from '../../../hasura-metadata-api/selectors';
import Skeleton from 'react-loading-skeleton';
import { FunctionName } from '../../../../metadata/types';

export type ModifyProps = {
  dataSourceName: string;
  qualifiedFunction: QualifiedFunction;
};

export const Modify = (props: ModifyProps) => {
  const { data: driverName, isFetching: isFetchingMetadata } = useMetadata(
    m =>
      m.metadata.sources.find(source => source.name === props.dataSourceName)
        ?.kind
  );

  const [isEditConfigurationModalOpen, setIsEditConfigurationModalOpen] =
    useState(false);
  const [isPermissionsEditorModalOpen, setIsPermissionsEditorModalOpen] =
    useState(false);
  const { data } = useMetadata(m => findSource(props.dataSourceName)(m));

  return (
    <div className="py-4">
      <div className="w-full bg-white p-4 rounded-sm border my-2">
        <div className="flex gap-2 mb-sm items-center">
          <div className="font-semibold text-2xl">Configuration</div>
          <IconTooltip message="allows you to customize any given function with a custom name and custom root fields of an already tracked function. This will replace the already present customization." />
        </div>

        <DisplayConfigurationDetails {...props} />
        <div className="flex gap-2 justify-end">
          <Button
            onClick={() => setIsEditConfigurationModalOpen(true)}
            icon={<FaEdit />}
          >
            Edit Configuration
          </Button>
          {data?.kind === 'snowflake' && (
            <Button
              onClick={() => setIsPermissionsEditorModalOpen(true)}
              icon={<FaKey />}
            >
              Edit Permissions
            </Button>
          )}
        </div>
        {isEditConfigurationModalOpen && (
          <ModifyFunctionConfiguration
            {...props}
            onSuccess={() => setIsEditConfigurationModalOpen(false)}
            onClose={() => setIsEditConfigurationModalOpen(false)}
          />
        )}
        {isPermissionsEditorModalOpen && (
          <ModifyFunctionPermissionsDialog
            dataSourceName={props.dataSourceName}
            qualifiedFunction={props.qualifiedFunction as FunctionName[]}
            onClose={() => setIsPermissionsEditorModalOpen(false)}
          />
        )}

        {isFetchingMetadata && <Skeleton width={80} height={60} />}

        {!isFetchingMetadata && (
          <div>
            <FunctionGraphQLCustomization
              driver={driverName as SupportedDrivers}
              dataSourceName={props.dataSourceName}
              qualifiedFunction={props.qualifiedFunction}
            />
          </div>
        )}
      </div>
    </div>
  );
};
