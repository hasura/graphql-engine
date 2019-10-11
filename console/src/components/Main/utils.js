const notificationVersionBlacklist = ['v1.0.0-beta.7'];

export const showVersionUpdate = version => {
  if (notificationVersionBlacklist.find((v) => v !== version)) {
    return true;
  }
  return false;
};
