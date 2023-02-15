export const globalTypes = {
  consoleType: {
    name: 'Console type',
    description: 'Console type to contextualise components',
    defaultValue: 'oss',
    options: ['oss', 'pro-lite', 'pro', 'cloud', 'cloud-pro'],
    control: {
      type: 'select',
      labels: {
        oss: 'OSS',
        'pro-lite': 'EE Lite',
        pro: 'EE',
        cloud: 'Cloud',
        'cloud-pro': 'Cloud EE',
      },
    },
    table: {
      type: { summary: 'string' },
      defaultValue: { summary: 'OSS' },
    },
  },
  adminSecretSet: {
    name: 'Admin secret set',
    description:
      'Admin secret set, if true, a adminSecret will be also available',
    defaultValue: false,
    control: { type: 'boolean' },
    table: {
      type: { summary: 'boolean' },
      defaultValue: { summary: 'false' },
    },
  },
};
