import { useState } from 'react';
import { FaAngleDown, FaAngleUp, FaExclamationTriangle } from 'react-icons/fa';
import { InconsistentObject } from '../../../../hasura-metadata-api';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';

export const InconsistentSourceDetails = ({
  inconsistentSource,
}: {
  inconsistentSource: InconsistentObject;
}) => {
  const [isExpanded, setIsExpanded] = useState(false);

  return (
    <div className="flex justify-between">
      <div className="max-w-xl">
        {!isExpanded ? (
          <div className="flex gap-2 items-center">
            <FaExclamationTriangle className="text-red-500" />
            Source is inconsistent
          </div>
        ) : (
          <div>
            <IndicatorCard
              status="negative"
              headline={inconsistentSource.reason}
            >
              <pre className="whitespace-pre-line">
                {JSON.stringify(inconsistentSource.message)}
              </pre>
            </IndicatorCard>
          </div>
        )}
      </div>

      <div
        onClick={() => setIsExpanded(!isExpanded)}
        className="cursor-pointer font-semibold flex items-center gap-2"
      >
        {isExpanded ? (
          <>
            <FaAngleUp />
            Hide
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
};
