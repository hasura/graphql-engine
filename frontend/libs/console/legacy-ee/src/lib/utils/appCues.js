import globals from '../Globals';

export const notifyRouteChangeToAppcues = () => {
  if (window.Appcues) {
    window.Appcues.page();
  }
};

export const appcuesIdentify = () => {
  if (window.Appcues) {
    window.Appcues.identify(globals.hasuraCloudProjectId);
  }
};
