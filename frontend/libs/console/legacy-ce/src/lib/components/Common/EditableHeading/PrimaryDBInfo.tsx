import React from 'react';
import { IconTooltip } from '../../../new-components/Tooltip';
import { useMetadataSource } from '../../../features/MetadataAPI';
import { MetadataDataSource } from '../../../metadata/types';
import { FaExclamationTriangle } from 'react-icons/fa';

type Props = {
  source: string;
};

const listSecondarySourceTypes = (source: MetadataDataSource) => {
  const { connection_set, read_replicas } = source?.configuration || {};
  const secSources = [];
  if (read_replicas && read_replicas?.length) {
    // read replicas are configured.
    secSources.push('Read-Replicas');
  }
  if (connection_set && connection_set?.length) {
    // when connection_set has one or more configurations, the data will be used from primary.
    secSources.push('Dynamic Routing');
  }
  return secSources;
};

export const PrimaryDBInfo = (props: Props) => {
  const { source } = props;
  const { data: sourceDefinition } = useMetadataSource(source);
  if (sourceDefinition && listSecondarySourceTypes(sourceDefinition).length) {
    return (
      <div className="flex items-center text-gray-500 font-thin mb-formlabel text-lg">
        <IconTooltip
          icon={
            <FaExclamationTriangle className="pr-2 text-xl text-secondary" />
          }
          side="bottom"
          message={`This message is being displayed as you have ${listSecondarySourceTypes(
            sourceDefinition
          ).join(' and ')} enabled.`}
        />
        This page uses only the primary connection for database queries
      </div>
    );
  }
  return null;
};
