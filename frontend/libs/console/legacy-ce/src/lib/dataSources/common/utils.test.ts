import { getQueryName } from './utils';

describe('getQueryName', () => {
  describe('when no customizations', () => {
    it('returns the query name', () => {
      const result = getQueryName({
        column: {
          name: 'name',
          columns: ['name'],
        },
        tableConfiguration: {
          custom_name: '',
        },
        dataSourceCustomization: {},
      });
      expect(result).toEqual('name');
    });
  });

  describe('when table configuration is present', () => {
    it('returns the query name', () => {
      const result = getQueryName({
        column: {
          name: 'name',
          columns: ['name'],
        },
        tableConfiguration: {
          custom_name: 'my_name',
          column_config: {
            name: {
              custom_name: 'my_custom_name',
            },
          },
        },
        dataSourceCustomization: {},
      });
      expect(result).toEqual('my_custom_name');
    });
  });

  describe('when data source customization are present', () => {
    it('returns the query name', () => {
      const result = getQueryName({
        column: {
          name: 'name',
          columns: ['name'],
        },
        tableConfiguration: {
          custom_name: '',
        },
        dataSourceCustomization: {
          root_fields: {
            prefix: 'pre_',
            suffix: '_suf',
            namespace: 'ns',
          },
        },
      });
      expect(result).toEqual('pre_name_suf');
    });
  });

  describe('when table configuration and data source customization are present', () => {
    it('returns the query name', () => {
      const result = getQueryName({
        column: {
          name: 'name',
          columns: ['name'],
        },
        tableConfiguration: {
          custom_name: 'my_name',
          column_config: {
            name: {
              custom_name: 'my_custom_name',
            },
          },
        },
        dataSourceCustomization: {
          root_fields: {
            prefix: 'pre_',
            suffix: '_suf',
            namespace: 'ns',
          },
        },
      });
      expect(result).toEqual('pre_my_custom_name_suf');
    });
  });
});
