export type EELicenseType = 'trial' | 'paid';
export type EELiteAccess =
  | {
      access: 'forbidden';
    }
  | {
      access: 'loading';
    }
  | {
      access: 'active';
      license: EELicenseInfo;
      expires_at: Date;
      kind: 'default' | 'grace';
    }
  | {
      access: 'expired';
      license: EELicenseInfo;
    }
  | {
      access: 'deactivated';
      license: EELicenseInfo;
    }
  | {
      access: 'eligible';
    };

export type EELicenseInfo =
  | {
      status: 'active' | 'expired' | 'deactivated';
      type: EELicenseType;
      expiry_at: Date;
      grace_at?: Date;
    }
  | {
      status: 'none';
      type: EELicenseType;
      expiry_at?: Date;
      grace_at?: Date;
    };
export type EELiteAccessStatus = EELiteAccess['access'];

export type EETrialRegistrationResponse = {
  registerEETrial: {
    client_id: string;
    client_secret: string;
  };
};
