export type PluginOptions = {
  prodKey: string;
  devKey: string;
  trackPage: false;
  trackPageDelay: 50;
};

export type Options = Partial<PluginOptions>;