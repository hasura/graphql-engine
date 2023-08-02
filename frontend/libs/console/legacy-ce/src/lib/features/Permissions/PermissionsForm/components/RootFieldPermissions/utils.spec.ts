import { TableColumn } from '../../../../DataSource';
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
        false
      );
    });
  });
  describe('when pk is selected', () => {
    it('it returns true', () => {
      const tableColumns = [
        { name: 'AlbumId', isPrimaryKey: true },
      ] as TableColumn[];
      expect(hasSelectedPrimaryKey({ AlbumId: true }, tableColumns)).toEqual(
        true
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
  describe('root permissions is null', () => {
    it('returns disabled and checked', () => {
      const args: PermissionCheckboxStateArg = {
        permission: '',
        hasEnabledAggregations: false,
        hasSelectedPrimaryKeys: false,
        isSubscriptionStreamingEnabled: false,
        rootPermissions: null,
      };
      expect(getPermissionCheckboxState(args)).toEqual({
        disabled: true,
        checked: true,
      });
    });
  });

  describe('when permission is select', () => {
    describe('when permission is in root permissions', () => {
      it('returns default state', () => {
        const args: PermissionCheckboxStateArg = {
          rootPermissions: ['select'],
          permission: 'select',
          hasEnabledAggregations: false,
          hasSelectedPrimaryKeys: false,
          isSubscriptionStreamingEnabled: false,
        };
        expect(getPermissionCheckboxState(args)).toEqual({
          disabled: false,
          checked: true,
        });
      });
    });
  });
  describe('when permission is NOT in root permissions', () => {
    it('returns default state', () => {
      const args: PermissionCheckboxStateArg = {
        rootPermissions: ['select_by_pk'],
        permission: 'select',
        hasEnabledAggregations: false,
        hasSelectedPrimaryKeys: false,
        isSubscriptionStreamingEnabled: false,
      };
      expect(getPermissionCheckboxState(args)).toEqual({
        disabled: false,
        checked: false,
      });
    });
  });
});

describe('getSelectByPkCheckboxState', () => {
  it.each`
    rootPermissions     | permission        | hasSelectedPrimaryKeys | expected
    ${['select_by_pk']} | ${'select_by_pk'} | ${false}               | ${{ checked: false, disabled: true, title: 'Allow access to the table primary key column(s) first' }}
    ${['select_by_pk']} | ${'select_by_pk'} | ${true}                | ${{ checked: true, disabled: false, title: '' }}
    ${['select']}       | ${'select_by_pk'} | ${true}                | ${{ checked: false, disabled: false, title: '' }}
  `(
    'returns the select_by_pk checkbox state for rootPermissions $rootPermissions, permission $permission, hasSelectedPrimaryKeys $hasSelectedPrimaryKeys',
    ({ rootPermissions, permission, hasSelectedPrimaryKeys, expected }) => {
      expect(
        getSelectByPkCheckboxState({
          hasSelectedPrimaryKeys,
          rootPermissions,
          permission,
        })
      ).toEqual(expected);
    }
  );
});

describe('getSelectStreamCheckboxState', () => {
  it.each`
    rootPermissions      | permission         | isSubscriptionStreamingEnabled | expected
    ${['select_stream']} | ${'select_stream'} | ${false}                       | ${{ checked: true, disabled: true, title: 'Enable the streaming subscriptions experimental feature first' }}
    ${['select_stream']} | ${'select_by_pk'}  | ${false}                       | ${{ checked: false, disabled: true, title: 'Enable the streaming subscriptions experimental feature first' }}
    ${['select_stream']} | ${'select_by_pk'}  | ${true}                        | ${{ checked: false, disabled: false, title: '' }}
  `(
    'returns the select_stream checkbox state for rootPermissions $rootPermissions, permission $permission, isSubscriptionStreamingEnabled $isSubscriptionStreamingEnabled',
    ({
      rootPermissions,
      permission,
      isSubscriptionStreamingEnabled,
      expected,
    }) => {
      expect(
        getSelectStreamCheckboxState({
          isSubscriptionStreamingEnabled,
          rootPermissions,
          permission,
        })
      ).toEqual(expected);
    }
  );
});

describe('getSelectAggregateCheckboxState', () => {
  it.each`
    rootPermissions      | permission         | hasEnabledAggregations | expected
    ${['select_stream']} | ${'select_stream'} | ${false}               | ${{ checked: false, disabled: true, title: 'Enable aggregation queries permissions first' }}
    ${['select_stream']} | ${'select_stream'} | ${true}                | ${{ checked: true, disabled: false, title: '' }}
    ${['select_stream']} | ${'select'}        | ${true}                | ${{ checked: false, disabled: false, title: '' }}
  `(
    'returns the select_stream checkbox state for rootPermissions $rootPermissions, permission $permission, hasEnabledAggregations $hasEnabledAggregations',
    ({ rootPermissions, permission, hasEnabledAggregations, expected }) => {
      expect(
        getSelectAggregateCheckboxState({
          hasEnabledAggregations,
          rootPermissions,
          permission,
        })
      ).toEqual(expected);
    }
  );
});
