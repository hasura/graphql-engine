const name = {
  label: 'Name',
  key: 'key',
  type: 'text',
};

const service_account = {
  key: 'service_account',
  label: 'Connection Details',
  type: 'radio-group-with-inputs',
  options: [
    {
      key: 'env_var',
      label: 'Enviroment Variable',
      fields: [
        {
          label: 'Env var',
          key: 'env_var',
          type: 'text',
          placeholder: 'SERVICE_ACCOUNT_KEY_FROM_ENV',
        },
      ],
    },
    {
      key: 'connection_parameters',
      label: 'Connection Parameters',
      fields: [
        {
          label: 'Service Account Key',
          key: 'service_account_key',
          type: 'json',
        },
      ],
      expand_keys: true,
    },
  ],
};

const configuration = {
  key: 'configuration',
  label: 'Connect Database Via',
  fields: [
    service_account,
    {
      label: 'Project Id',
      key: 'project_id',
      type: 'text',
      placeholder: 'project_id',
    },
    {
      label: 'Datasets',
      key: 'datasets',
      type: 'text',
      placeholder: 'dataset_1, dataset_2',
    },
    {
      label: 'Global Select Limit',
      key: 'global_select_limit',
      type: 'number',
      placeholder: 1000,
    },
  ],
};

const customization = {
  key: 'customization',
  label: 'GraphQL Customization',
  fields: [
    {
      label: 'Namespace',
      key: 'root_fields.namespace',
      type: 'text',
    },
    {
      label: 'Prefix',
      key: 'root_fields.prefix',
      type: 'text',
    },
    {
      label: 'Suffix',
      key: 'root_fields.suffix',
      type: 'text',
    },
    {
      label: 'Prefix',
      key: 'type_names.prefix',
      type: 'text',
    },
    {
      label: 'Suffix',
      key: 'type_names.suffix',
      type: 'text',
    },
  ],
};

export const getUISchema = async () => {
  return [name, configuration, customization];
};
