import clsx from 'clsx';
import { useState } from 'react';
import { FaAngleDown, FaAngleUp, FaExclamationTriangle } from 'react-icons/fa';
import { Feature } from '../../../../DataSource';
import { InconsistentObject } from '../../../../hasura-metadata-api';
import { InconsistentSourceDetails } from './InconsistentSourceDetails';

export const DisplayDetails = ({
  details,
}: {
  details: {
    version: string | Feature.NotImplemented;
  };
}) => {
  const [isExpanded, setIsExpanded] = useState(false);

  const { version } = details;

  if (version === Feature.NotImplemented) return null;

  if (version)
    return (
      <div className="flex justify-start">
        <div
          className={clsx(
            'max-w-md',
            isExpanded
              ? 'whitespace-pre-line max-w-xl'
              : 'overflow-hidden text-ellipsis whitespace-nowrap'
          )}
          title={version}
        >
          <b>Version: </b>
          {version}
        </div>

        <div
          onClick={() => setIsExpanded(!isExpanded)}
          className="cursor-pointer font-semibold flex items-center gap-2"
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
        </div>
      </div>
    );

  return (
    <div className="flex gap-2 items-center">
      <FaExclamationTriangle className="text-yellow-500" /> Could not fetch
      version info.
    </div>
  );
};

export const Details = ({
  dataSourceName,
  details,
  inconsistentSources,
}: {
  dataSourceName: string;
  details: {
    version: string | Feature.NotImplemented;
  };
  inconsistentSources: InconsistentObject[];
}) => {
  const inconsistentSource = inconsistentSources.find(
    source => source.definition === dataSourceName
  );

  if (inconsistentSource)
    return (
      <InconsistentSourceDetails inconsistentSource={inconsistentSource} />
    );

  return <DisplayDetails details={details} />;
};
