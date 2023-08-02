import React from 'react';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import { Link } from 'react-router';

type InconsistentBadgeProps = {
  inconsistencyDetails: any;
};

export const InconsistentBadge = ({
  inconsistencyDetails,
}: InconsistentBadgeProps) => {
  return (
    <div className="mt-6 w-full sm:w-9/12">
      <IndicatorCard
        status="negative"
        headline="This remote schema is in an inconsistent state."
      >
        <div>
          <div>
            <b>Reason:</b> {inconsistencyDetails.reason}
          </div>
          <div>
            <i>
              (Please resolve the inconsistencies and reload the remote schema
              from <Link to="/settings/metadata-status">here</Link>. Fields from
              this remote schema are currently not exposed over the GraphQL API)
            </i>
          </div>
        </div>
      </IndicatorCard>
    </div>
  );
};
