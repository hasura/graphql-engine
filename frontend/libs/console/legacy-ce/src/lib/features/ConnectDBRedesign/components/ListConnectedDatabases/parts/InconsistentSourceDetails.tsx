import { useState } from 'react';
import { FaAngleDown, FaAngleUp, FaExclamationTriangle } from 'react-icons/fa';
import { IndicatorCard } from '@hasura/shared/ui';
import { InconsistentObject } from '@hasura/shared/types';
import { Flex } from '@radix-ui/themes';

export const InconsistentSourceDetails = ({
  inconsistentSource,
}: {
  inconsistentSource: InconsistentObject;
}) => {
  const [isExpanded, setIsExpanded] = useState(false);

  return (
    <Flex justify="between">
      <div className="max-w-xl">
        {!isExpanded ? (
          <Flex gap="2" align="center">
            <FaExclamationTriangle className="text-red-500" />
            Source is inconsistent
          </Flex>
        ) : (
          <div>
            <IndicatorCard
              status="negative"
              headline={inconsistentSource.reason}
            >
              <pre className="whitespace-pre-line">
                {'message' in inconsistentSource
                  ? typeof inconsistentSource.message === 'string'
                    ? inconsistentSource.message
                    : JSON.stringify(inconsistentSource.message)
                  : ''}
              </pre>
            </IndicatorCard>
          </div>
        )}
      </div>

      <Flex
        onClick={() => setIsExpanded(!isExpanded)}
        align="center"
        gap="2"
        className="cursor-pointer font-semibold"
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
      </Flex>
    </Flex>
  );
};
