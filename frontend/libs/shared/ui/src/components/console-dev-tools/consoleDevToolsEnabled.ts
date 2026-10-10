export const consoleDevToolsEnabled = () => {
  const explicitlyDisabled =
    window.location.search.includes('console-dev-tools=false') ||
    window.localStorage.getItem('console-dev-tools') === 'false';
  const explicitlyEnabled =
    window.location.search.includes('console-dev-tools=true') ||
    window.localStorage.getItem('console-dev-tools') === 'true';
  if (
    !explicitlyEnabled &&
    (process.env.NODE_ENV !== 'development' || explicitlyDisabled)
  ) {
    return false;
  }
  return true;
};
