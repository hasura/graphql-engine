export const needsOldManageTableUI = (driver: string) => {
  return ['postgres', 'pg', 'mssql', 'citus', 'cockroach', 'alloy'].includes(
    driver
  );
};
