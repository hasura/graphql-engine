import { IconTooltip } from '@hasura/shared/ui';
import { getLSItem, setLSItem } from '@hasura/shared/utils';
import {
  BreakingChangesColor,
  BreakingChangesTooltipMessage,
  DangerousChangesColor,
  DangerousChangesTooltipMessage,
  DefaultToolTipMessage,
  SafeChangesColor,
  SafeChangesTooltipMessage,
} from '../../SchemaRegistry/constants';
import { useGetSchemaRegistryNotificationColor } from '../../SchemaRegistry/hooks/useGetSchemaRegistryNotificationColor';
import { LS_KEYS } from '@hasura/shared/types';
import { useAppContext } from '@hasura/shared/context';
import { FaExclamationCircle } from 'react-icons/fa';
import { FaRegMap } from 'react-icons/fa6';
import { useEffect } from 'react';

const SchemaRegistryBadge = () => {
  const { envVars } = useAppContext();

  const projectID = envVars.projectID || '';
  const fetchSchemaRegistryNotificationData =
    useGetSchemaRegistryNotificationColor(projectID);

  let color = '';
  let tooltipMessage = DefaultToolTipMessage;
  let change_recorded_at = '';
  let showNotifications = false;

  if (fetchSchemaRegistryNotificationData.kind === 'success') {
    const data =
      fetchSchemaRegistryNotificationData?.response?.schema_registry_dumps_v2 ||
      [];
    if (
      data.length &&
      data[0].diff_with_previous_schema &&
      data[0].diff_with_previous_schema[0] &&
      data[0].diff_with_previous_schema[0].schema_diff_data &&
      data[0].change_recorded_at
    ) {
      const changes = data[0].diff_with_previous_schema[0].schema_diff_data;
      // Check if there's a change with a criticality level of "BREAKING"
      const hasBreakingChange = changes.some(
        (change) =>
          change.criticality && change.criticality.level === 'BREAKING',
      );
      const hasDangerousChange = changes.some(
        (change) =>
          change.criticality && change.criticality.level === 'DANGEROUS',
      );
      const last_viewed_change = getLSItem(LS_KEYS.lastViewedSchemaChange);

      if (
        (!last_viewed_change ||
          last_viewed_change < data[0].change_recorded_at) &&
        changes
      ) {
        if (hasBreakingChange) {
          color = BreakingChangesColor;
          tooltipMessage = BreakingChangesTooltipMessage;
        } else if (hasDangerousChange) {
          //gold color instead of yellow to be more visible
          color = DangerousChangesColor;
          tooltipMessage = DangerousChangesTooltipMessage;
        } else {
          color = SafeChangesColor;
          tooltipMessage = SafeChangesTooltipMessage;
        }
        change_recorded_at = data[0].change_recorded_at;
        showNotifications = true;
      }
    }
  }

  useEffect(() => {
    if (showNotifications && change_recorded_at) {
      setLSItem(LS_KEYS.lastViewedSchemaChange, change_recorded_at);
    }
  }, [showNotifications, change_recorded_at]);

  return (
    <IconTooltip
      icon={color ? <FaExclamationCircle style={{ color }} /> : <FaRegMap />}
      message={tooltipMessage}
    />
  );
};

export default SchemaRegistryBadge;
