import clsx from 'clsx';
import { Flex } from '@radix-ui/themes';
import { useState } from 'react';
import { FaAngleDown, FaAngleUp, FaExclamationTriangle } from 'react-icons/fa';
import { InconsistentSourceDetails } from './InconsistentSourceDetails';
import { InconsistentObject } from '@hasura/shared/types';
import { findInconsistentSource } from '@hasura/metadata/helpers';

export const DisplayDetails = ({
  details,
  isSupported,
}: {
  details: {
    version: string;
  };
  isSupported: boolean;
}) => {
  const [isExpanded, setIsExpanded] = useState(false);

  const { version } = details;

  if (!isSupported) return null;

  if (version)
    return (
      <Flex justify="start">
        <div
          className={clsx(
            'max-w-md',
            isExpanded
              ? 'whitespace-pre-line max-w-xl'
              : 'overflow-hidden text-ellipsis whitespace-nowrap',
          )}
          title={version}
        >
          <b>Version: </b>
          {version}
        </div>

        <Flex
          align="center"
          gap="2"
          onClick={() => setIsExpanded(!isExpanded)}
          className="cursor-pointer font-semibold"
        >
          {isExpanded ? (
            <>
              <FaAngleUp />
              Less
            </>
          ) : (
            <>
              <FaAngleDown />
              More
            </>
          )}
        </Flex>
      </Flex>
    );

  return (
    <Flex gap="2" align="center">
      <FaExclamationTriangle className="text-yellow-500" /> Could not fetch
      version info.
    </Flex>
  );
};

export const Details = ({
  dataSourceName,
  details,
  inconsistentSources,
  isSupported,
}: {
  dataSourceName: string;
  details: {
    version: string;
  };
  inconsistentSources: InconsistentObject[];
  isSupported: boolean;
}) => {
  const inconsistentSource = findInconsistentSource(
    inconsistentSources,
    dataSourceName,
  );
  if (inconsistentSource)
    return (
      <InconsistentSourceDetails inconsistentSource={inconsistentSource} />
    );

  return <DisplayDetails details={details} isSupported={isSupported} />;
};
