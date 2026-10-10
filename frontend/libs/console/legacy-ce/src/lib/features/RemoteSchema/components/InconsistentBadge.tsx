import { IndicatorCard, RelativeLink } from '@hasura/shared/ui';

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
              from{' '}
              <RelativeLink to="/settings/metadata-status">here</RelativeLink>.
              Fields from this remote schema are currently not exposed over the
              GraphQL API)
            </i>
          </div>
        </div>
      </IndicatorCard>
    </div>
  );
};
