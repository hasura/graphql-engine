/**
 * At the time of writing, the get_catalog_state request is not typed at all in the Console.
 */
export const get_catalog_state = {
  id: 'b23fe106-4c3e-405f-9ca5-4aef77dec759',
  cli_state: {
    settings: { migration_mode: 'true' },
    isStateCopyCompleted: true,
  },
  console_state: {
    telemetryNotificationShown: true,
    onboardingShown: true,
    console_notifications: {
      admin: { read: [], showBadge: true, date: null },
    },
  },
};
