module.exports = () => config => {
  config.plugins = config.plugins.map(plugin => {
    if (!plugin.definitions || !plugin.definitions['process.env']) {
      return plugin;
    }

    if (plugin.definitions['process.env']['NX_CLOUD_ACCESS_TOKEN']) {
      delete plugin.definitions['process.env']['NX_CLOUD_ACCESS_TOKEN'];
    }
    return plugin;
  });
  return config;
};
