import { CardedTable } from '../../../../new-components/CardedTable';
import { Switch } from '../../../../new-components/Switch';
import { Analytics, REDACT_EVERYTHING } from '../../../Analytics';
import React from 'react';
import { useFeatureFlags } from '../../hooks/useFeatureFlags';
import { useSetFeatureFlagEnabled } from '../../hooks/useSetFeatureFlagEnabled';
import { FeatureFlagDefinition, FeatureFlagType } from '../../types';

interface FeatureCellProps {
  title: string;
  description: string;
}

const FeatureCell = (props: FeatureCellProps) => {
  const { description, title } = props;
  return (
    <div className="pr-4">
      <div className="text-gray-600 font-semibold break-all">{title}</div>
      <div className="text-gray-600 whitespace-normal">{description}</div>
    </div>
  );
};

const columns = [null, 'Feature', 'section', 'status'];

const formatData = (
  data: Array<FeatureFlagType>,
  mutation: ReturnType<typeof useSetFeatureFlagEnabled>
): React.ReactNode[][] =>
  data.map(item => [
    <Switch
      checked={item.state.enabled}
      onCheckedChange={() =>
        mutation.mutate({ flagId: item.id, newState: !item.state.enabled })
      }
    />,
    <FeatureCell title={item.title} description={item.description} />,
    item.section,
    item.status,
  ]);

interface FeatureFlagsProps {
  additionalFlags?: FeatureFlagDefinition[];
}

const FeatureFlagsBody = (props: FeatureFlagsProps) => {
  const { additionalFlags } = props;
  const result = useFeatureFlags(additionalFlags);
  const setFeatureFlagEnabled = useSetFeatureFlagEnabled();
  const { isError, isLoading, data } = result;
  if (isLoading) return <h3 className="text-lg">Loading...</h3>;
  if (isError)
    return (
      <h3 className="text-lg">
        There was an error while loading. <br />
        Please try again later.
      </h3>
    );
  if (data && data?.length > 0) {
    return (
      <CardedTable
        columns={columns}
        data={formatData(data, setFeatureFlagEnabled)}
      />
    );
  }
  return (
    <h3 className="text-lg">There are currently no Feature flags available.</h3>
  );
};

export const FeatureFlags = (props: FeatureFlagsProps) => {
  const { additionalFlags } = props;
  return (
    <Analytics name="FeatureFlags" {...REDACT_EVERYTHING}>
      <div className="p-4 bootstrap-jail">
        <h2 className="text-xl font-semibold mb-3.5">Feature Flags</h2>
        <p>
          Feature flags enable experimental features in Console. <br />
          These features may be actively in development or in beta status.
        </p>
        <div className="mb-10" />
        <FeatureFlagsBody additionalFlags={additionalFlags} />
      </div>
    </Analytics>
  );
};
