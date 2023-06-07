function config(entry = []) {
  return [...entry, require.resolve('./src/lib/preset/preview')];
}

function managerEntries(entry = []) {
  return [...entry, require.resolve('./src/lib/preset/manager')];
}

module.exports = {
  config,
  managerEntries,
  previewBody: body => `
    ${body}
  <script>
    // Working as iframe and parent are on the same domain
    const params = new URL(window.parent.location)?.searchParams;
    let consoleType = 'oss';
    let adminSecretSet = false;

    const updateGlobals = () => {
      if (params?.get('globals')) {
        consoleType =
          params
            .get('globals')
            .split(';')
            .filter(global => global.includes('consoleType'))[0]
            ?.split(':')[1] || 'oss';
        adminSecretSet =
          params
            .get('globals')
            .split(';')
            .filter(global => global.includes('adminSecretSet'))[0]
            ?.split(':')[1] === '!true';
      }

      window.__env = {
        dataApiUrl: 'http://localhost:8080',
        apiHost: 'http://localhost',
        apiPort: '8080',
        nodeEnv: 'development',
        consoleType: consoleType === 'cloud-pro' ? 'cloud' : consoleType,
        adminSecretSet,
        adminSecret: adminSecretSet ? '*******' : null,
        tenantID: consoleType === 'cloud-pro' ? 'tenant-id' : null,
      };
    };

    updateGlobals();
  </script>
  `,
};
