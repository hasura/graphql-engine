import { TableColumn } from '@hasura/metadata/data-source';
import {
  getSectionStatusLabel,
  SectionLabelProps,
  getPermissionCheckboxState,
  PermissionCheckboxStateArg,
  getSelectByPkCheckboxState,
  getSelectStreamCheckboxState,
  getSelectAggregateCheckboxState,
  hasSelectedPrimaryKey,
} from './utils';

describe('hasSelectedPrimaryKey', () => {
  describe('when pk is not selected', () => {
    it('it returns false', () => {
      const tableColumns = [
        { name: 'AlbumId', isPrimaryKey: true },
      ] as TableColumn[];
      expect(hasSelectedPrimaryKey({ AlbumId: false }, tableColumns)).toEqual(
        false,
      );
    });
  });
  describe('when pk is selected', () => {
    it('it returns true', () => {
      const tableColumns = [
        { name: 'AlbumId', isPrimaryKey: true },
      ] as TableColumn[];
      expect(hasSelectedPrimaryKey({ AlbumId: true }, tableColumns)).toEqual(
        true,
      );
    });
  });
});

describe('getSectionStatusLabel', () => {
  describe('when permissions are null', () => {
    it('returns "all enabled"', () => {
      const args: SectionLabelProps = {
        subscriptionRootPermissions: null,
        queryRootPermissions: null,
        hasEnabledAggregations: false,
        hasSelectedPrimaryKeys: false,
        isSubscriptionStreamingEnabled: false,
      };
      expect(getSectionStatusLabel(args)).toEqual('  - all enabled');
    });
  });

  describe('when permissions are empty', () => {
    it('returns "all disabled"', () => {
      const args: SectionLabelProps = {
        subscriptionRootPermissions: [],
        queryRootPermissions: [],
        hasEnabledAggregations: false,
        hasSelectedPrimaryKeys: false,
        isSubscriptionStreamingEnabled: false,
      };
      expect(getSectionStatusLabel(args)).toEqual('  - all disabled');
    });
  });

  describe('when permissions are non empty', () => {
    it('returns "partially disabled"', () => {
      const args: SectionLabelProps = {
        subscriptionRootPermissions: [
          'select',
          'select_by_pk',
          'select_aggregate',
          'select_stream',
        ],
        queryRootPermissions: [],
        hasEnabledAggregations: false,
        hasSelectedPrimaryKeys: false,
        isSubscriptionStreamingEnabled: false,
      };
      expect(getSectionStatusLabel(args)).toEqual('  - partially enabled');
    });
  });

  describe('when permissions are all selected', () => {
    it('returns "all enabled"', () => {
      const args: SectionLabelProps = {
        subscriptionRootPermissions: [
          'select',
          'select_by_pk',
          'select_aggregate',
          'select_stream',
        ],
        queryRootPermissions: ['select', 'select_by_pk', 'select_aggregate'],
        hasEnabledAggregations: true,
        hasSelectedPrimaryKeys: true,
        isSubscriptionStreamingEnabled: true,
      };
      expect(getSectionStatusLabel(args)).toEqual('  - all enabled');
    });
  });

  describe('when aggregations are not selected', () => {
    describe('when permissions are all selected', () => {
      it('returns "all enabled"', () => {
        const args: SectionLabelProps = {
          subscriptionRootPermissions: [
            'select',
            'select_by_pk',
            'select_stream',
          ],
          queryRootPermissions: ['select', 'select_by_pk'],
          hasEnabledAggregations: false,
          hasSelectedPrimaryKeys: true,
          isSubscriptionStreamingEnabled: true,
        };
        expect(getSectionStatusLabel(args)).toEqual('  - all enabled');
      });
    });
    describe('when some permissions are selected', () => {
      it('returns "partially enabled"', () => {
        const args: SectionLabelProps = {
          subscriptionRootPermissions: ['select'],
          queryRootPermissions: ['select'],
          hasEnabledAggregations: false,
          hasSelectedPrimaryKeys: true,
          isSubscriptionStreamingEnabled: true,
        };
        expect(getSectionStatusLabel(args)).toEqual('  - partially enabled');
      });
    });
  });

  describe('when primary keys are not selected', () => {
    describe('when permissions are all selected', () => {
      it('returns "all enabled"', () => {
        const args: SectionLabelProps = {
          subscriptionRootPermissions: [
            'select',
            'select_aggregate',
            'select_stream',
          ],
          queryRootPermissions: ['select', 'select_aggregate'],
          hasEnabledAggregations: true,
          hasSelectedPrimaryKeys: false,
          isSubscriptionStreamingEnabled: true,
        };
        expect(getSectionStatusLabel(args)).toEqual('  - all enabled');
      });
    });
    describe('when some permissions are selected', () => {
      it('returns "partially enabled"', () => {
        const args: SectionLabelProps = {
          subscriptionRootPermissions: ['select'],
          queryRootPermissions: ['select'],
          hasEnabledAggregations: false,
          hasSelectedPrimaryKeys: false,
          isSubscriptionStreamingEnabled: true,
        };
        expect(getSectionStatusLabel(args)).toEqual('  - partially enabled');
      });
    });
  });

  describe('when subscription streaming is not selected', () => {
    describe('when permissions are all selected', () => {
      it('returns "all enabled"', () => {
        const args: SectionLabelProps = {
          subscriptionRootPermissions: [
            'select',
            'select_by_pk',
            'select_aggregate',
          ],
          queryRootPermissions: ['select', 'select_by_pk', 'select_aggregate'],
          hasEnabledAggregations: true,
          hasSelectedPrimaryKeys: true,
          isSubscriptionStreamingEnabled: false,
        };
        expect(getSectionStatusLabel(args)).toEqual('  - all enabled');
      });
    });
    describe('when some permissions are selected', () => {
      it('returns "partially enabled"', () => {
        const args: SectionLabelProps = {
          subscriptionRootPermissions: ['select', 'select_by_pk'],
          queryRootPermissions: ['select', 'select_by_pk'],
          hasEnabledAggregations: true,
          hasSelectedPrimaryKeys: true,
          isSubscriptionStreamingEnabled: false,
        };
        expect(getSectionStatusLabel(args)).toEqual('  - partially enabled');
      });
    });
  });
});

describe('getPermissionCheckboxState', () => {
  // `checked` state is owned by the form (CheckboxesField bound via
  // react-hook-form); these utils only compute per-permission enablement
  // (`disabled` + tooltip `title`) based on the current prerequisites.
  it('leaves a permission without prerequisites enabled (e.g. "select")', () => {
    const args: PermissionCheckboxStateArg = {
      permission: 'select',
      hasEnabledAggregations: false,
      hasSelectedPrimaryKeys: false,
      isSubscriptionStreamingEnabled: false,
    };
    expect(getPermissionCheckboxState(args)).toEqual({ disabled: false });
  });

  it('disables "select_by_pk" until a primary key is selected', () => {
    const args: PermissionCheckboxStateArg = {
      permission: 'select_by_pk',
      hasEnabledAggregations: false,
      hasSelectedPrimaryKeys: false,
      isSubscriptionStreamingEnabled: false,
    };
    expect(getPermissionCheckboxState(args)).toEqual({
      disabled: true,
      title: 'Allow access to the table primary key column(s) first',
    });
  });

  it('disables "select_stream" until streaming subscriptions are enabled', () => {
    const args: PermissionCheckboxStateArg = {
      permission: 'select_stream',
      hasEnabledAggregations: false,
      hasSelectedPrimaryKeys: false,
      isSubscriptionStreamingEnabled: false,
    };
    expect(getPermissionCheckboxState(args)).toEqual({
      disabled: true,
      title: 'Enable the streaming subscriptions experimental feature first',
    });
  });

  it('disables "select_aggregate" until aggregation permissions are enabled', () => {
    const args: PermissionCheckboxStateArg = {
      permission: 'select_aggregate',
      hasEnabledAggregations: false,
      hasSelectedPrimaryKeys: false,
      isSubscriptionStreamingEnabled: false,
    };
    expect(getPermissionCheckboxState(args)).toEqual({
      disabled: true,
      title: 'Enable aggregation queries permissions first',
    });
  });
});

describe('getSelectByPkCheckboxState', () => {
  it.each`
    hasSelectedPrimaryKeys | expected
    ${false}               | ${{ disabled: true, title: 'Allow access to the table primary key column(s) first' }}
    ${true}                | ${{ disabled: false, title: '' }}
  `(
    'returns the select_by_pk checkbox state for hasSelectedPrimaryKeys $hasSelectedPrimaryKeys',
    ({ hasSelectedPrimaryKeys, expected }) => {
      expect(getSelectByPkCheckboxState({ hasSelectedPrimaryKeys })).toEqual(
        expected,
      );
    },
  );
});

describe('getSelectStreamCheckboxState', () => {
  it.each`
    isSubscriptionStreamingEnabled | expected
    ${false}                       | ${{ disabled: true, title: 'Enable the streaming subscriptions experimental feature first' }}
    ${true}                        | ${{ disabled: false, title: '' }}
  `(
    'returns the select_stream checkbox state for isSubscriptionStreamingEnabled $isSubscriptionStreamingEnabled',
    ({ isSubscriptionStreamingEnabled, expected }) => {
      expect(
        getSelectStreamCheckboxState({ isSubscriptionStreamingEnabled }),
      ).toEqual(expected);
    },
  );
});

describe('getSelectAggregateCheckboxState', () => {
  it.each`
    hasEnabledAggregations | expected
    ${false}               | ${{ disabled: true, title: 'Enable aggregation queries permissions first' }}
    ${true}                | ${{ disabled: false, title: '' }}
  `(
    'returns the select_aggregate checkbox state for hasEnabledAggregations $hasEnabledAggregations',
    ({ hasEnabledAggregations, expected }) => {
      expect(
        getSelectAggregateCheckboxState({ hasEnabledAggregations }),
      ).toEqual(expected);
    },
  );
});
