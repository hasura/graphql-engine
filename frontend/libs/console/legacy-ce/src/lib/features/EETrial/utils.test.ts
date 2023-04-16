import { transformEntitlementToAccess } from './utils';
import { EELicenseInfo } from './types';

describe('transformEntitlementToAccess', () => {
  it('for status none, has access eligible', () => {
    expect(
      transformEntitlementToAccess({
        status: 'none',
        type: 'trial',
      })
    ).toEqual({
      access: 'eligible',
    });
  });
  it('for status active, has access active with kind default', () => {
    const license: EELicenseInfo = {
      status: 'active',
      type: 'trial',
      expiry_at: new Date(new Date().getTime() + 200000000),
    };
    expect(transformEntitlementToAccess(license)).toEqual({
      access: 'active',
      expires_at: license['expiry_at'],
      license,
      kind: 'default',
    });
  });
  it('for status expired before grace, has access active with kind grace', () => {
    const license: EELicenseInfo = {
      status: 'expired',
      type: 'trial',
      expiry_at: new Date(new Date().getTime() - 200000000),
      grace_at: new Date(new Date().getTime() + 200000000),
    };
    expect(transformEntitlementToAccess(license)).toEqual({
      access: 'active',
      expires_at: license['grace_at'],
      license,
      kind: 'grace',
    });
  });
  it('for status expired without grace, has access expired', () => {
    const license: EELicenseInfo = {
      status: 'expired',
      type: 'trial',
      expiry_at: new Date(new Date().getTime() - 200000000),
    };
    expect(transformEntitlementToAccess(license)).toEqual({
      access: 'expired',
      license,
    });
  });
  it('for status expired after grace, has access expired', () => {
    const license: EELicenseInfo = {
      status: 'expired',
      type: 'trial',
      expiry_at: new Date(new Date().getTime() - 200000000),
      grace_at: new Date(new Date().getTime() - 100000000),
    };
    expect(transformEntitlementToAccess(license)).toEqual({
      access: 'expired',
      license,
    });
  });
  it('for status deactivated, has access deactivated', () => {
    const license: EELicenseInfo = {
      status: 'deactivated',
      type: 'trial',
      expiry_at: new Date(new Date().getTime() + 200000000),
      grace_at: new Date(new Date().getTime() + 300000000),
    };
    expect(transformEntitlementToAccess(license)).toEqual({
      access: 'deactivated',
      license,
    });
  });
});
