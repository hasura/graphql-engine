import { render, screen } from '@testing-library/react';

import {
  type Compatibility,
  IsFeatureEnabled,
  checkCompatibility,
} from './isFeatureEnabled';

import { type HasuraPlan, StoreProvider } from './store';

describe('checkCompatibility', () => {
  describe('Types testing and types/runtime match testing', () => {
    it('When passed with an `enabled` property, then the doMatch and doNotMatch should not contain extra properties', () => {
      // Arrange
      const hasuraPlan: HasuraPlan = { name: 'ce' };

      const compatibility = {
        // ↓ will be in doMatch and doNotMatch
        ce: 'enabled',

        // ↓ will NOT be in doMatch and doNotMatch
        ee: { withLicense: 'disabled', withoutLicense: 'disabled' },
      } as const satisfies Compatibility;

      const expectedResult: ReturnType<
        typeof checkCompatibility<typeof compatibility>
      > = {
        status: 'enabled',
        doMatch: { ce: true },
        doNotMatch: {},

        current: { hasuraPlan: { name: 'ce' } },
      };

      // Act
      const result = checkCompatibility(compatibility, { hasuraPlan });

      // Assert
      expect(result.doMatch).toEqual(expectedResult.doMatch);
      expect(result.doNotMatch).toEqual(expectedResult.doNotMatch);

      // Types testing
      // @ts-expect-error The error is expected because here we are checking that the value reflects the runtime value
      expect(result.doMatch.ee).toBe(undefined);
      expect(result.doMatch).not.toHaveProperty('ee');
    });

    it('When passed with some `enabled` properties, then the doMatch and doNotMatch should not contain extra properties', () => {
      // Arrange
      const hasuraPlan: HasuraPlan = { name: 'ce' };

      const compatibility = {
        // ↓ will be in doMatch and doNotMatch
        ce: 'enabled',

        // ↓ will be in doMatch and doNotMatch
        ee: { withLicense: 'enabled', withoutLicense: 'enabled' },
      } as const satisfies Compatibility;

      const expectedResult: ReturnType<
        typeof checkCompatibility<typeof compatibility>
      > = {
        status: 'enabled',
        doMatch: { ce: true },
        doNotMatch: { ee: { withLicense: true, withoutLicense: true } },

        current: { hasuraPlan: { name: 'ce' } },
      };

      // Act
      const result = checkCompatibility(compatibility, { hasuraPlan });

      // Assert
      expect(result.doMatch).toEqual(expectedResult.doMatch);
      expect(result.doNotMatch).toEqual(expectedResult.doNotMatch);

      // Types testing
      expect(result.doMatch.ee).toBe(undefined);
      expect(result.doMatch).not.toHaveProperty('ee');
    });

    it('When passed with some `enabled` properties for the license, then the doMatch and doNotMatch should not contain extra properties', () => {
      // Arrange
      const hasuraPlan: HasuraPlan = { name: 'ce' };

      const compatibility = {
        ce: 'enabled', // <-- will be in doMatch and doNotMatch
        ee: {
          withLicense: 'enabled', // <-- will be in doMatch and doNotMatch
          withoutLicense: 'disabled', // <-- will NOT be in doMatch and doNotMatch
        },
      } as const satisfies Compatibility;

      const expectedResult: ReturnType<
        typeof checkCompatibility<typeof compatibility>
      > = {
        status: 'enabled',
        doMatch: { ce: true },
        doNotMatch: { ee: { withLicense: true } },

        current: { hasuraPlan: { name: 'ce' } },
      };

      // Act
      const result = checkCompatibility(compatibility, { hasuraPlan });

      // Assert
      expect(result.doMatch).toEqual(expectedResult.doMatch);
      expect(result.doNotMatch).toEqual(expectedResult.doNotMatch);

      // Types testing
      // @ts-expect-error The error is expected because here we are checking that the value reflects the runtime value
      expect(result.doNotMatch.ee?.withoutLicense).toBe(undefined);
      expect(result.doNotMatch.ee).not.toHaveProperty('withoutLicense');
    });

    it('When passed with all the license details as `disabled`, then the doMatch and doNotMatch should not contain them', () => {
      // Arrange
      const hasuraPlan: HasuraPlan = { name: 'ce' };

      const compatibility = {
        ce: 'disabled',

        // ↓ will NOT be in doMatch and doNotMatch
        ee: {
          withLicense: 'disabled',
          withoutLicense: 'disabled',
        },
      } as const satisfies Compatibility;

      const expectedResult: ReturnType<
        typeof checkCompatibility<typeof compatibility>
      > = {
        status: 'disabled',
        doMatch: {},
        doNotMatch: {},

        current: { hasuraPlan: { name: 'ce' } },
      };

      // Act
      const result = checkCompatibility(compatibility, { hasuraPlan });

      // Assert
      expect(result.doMatch).toEqual(expectedResult.doMatch);
      expect(result.doNotMatch).toEqual(expectedResult.doNotMatch);

      // Types testing
      // @ts-expect-error The error is expected because here we are checking that the value reflects the runtime value
      expect(result.doNotMatch.ee).toBe(undefined);
      expect(result.doNotMatch).not.toHaveProperty('ee');
    });
  });

  describe('Compatibility testing', () => {
    it('When passed with a CE plan, and the feature is enabled only in the CE plan, then return `enabled`', () => {
      // Arrange
      const hasuraPlan: HasuraPlan = { name: 'ce' };

      const compatibility = {
        ce: 'enabled',
        ee: { withLicense: 'disabled', withoutLicense: 'disabled' },
      } as const satisfies Compatibility;

      const expectedResult: ReturnType<
        typeof checkCompatibility<typeof compatibility>
      > = {
        status: 'enabled',
        doMatch: { ce: true },
        doNotMatch: {},

        current: { hasuraPlan: { name: 'ce' } },
      };

      // Act

      const result = checkCompatibility(compatibility, { hasuraPlan });

      // Assert
      expect(result).toEqual(expectedResult);

      // Types testing
      // @ts-expect-error The error is expected because here we are checking that the value reflects the runtime value
      expect(result.doMatch.ee).toBe(undefined);
      expect(result.doMatch).not.toHaveProperty('ee');
    });

    it('When passed with a EE plan with an active license, and the feature is enabled only in the EE plan with an active license, then return `enabled`', () => {
      const date = new Date('2100-03-10T14:42:52.775Z');

      // Arrange
      const hasuraPlan: HasuraPlan = {
        name: 'ee',
        license: {
          status: 'active',
          expiresAt: date,
          type: 'paid',
        },
      };

      const compatibility = {
        ce: 'disabled',
        ee: {
          withLicense: 'enabled',
          withoutLicense: 'disabled',
        },
      } as const satisfies Compatibility;

      const expectedResult: ReturnType<
        typeof checkCompatibility<typeof compatibility>
      > = {
        status: 'enabled',
        doMatch: { ee: { withLicense: true } },
        doNotMatch: {},

        current: {
          hasuraPlan: {
            license: {
              expiresAt: date,
              status: 'active',
              type: 'paid',
            },
            name: 'ee',
          },
        },
      };

      // Act
      const result = checkCompatibility(compatibility, { hasuraPlan });

      // Assert
      expect(result).toEqual(expectedResult);
    });
  });
});

describe('isFeatureEnabled', () => {
  describe('Types testing', () => {
    it('When passed with some `enabled` properties for the license, then the doMatch and doNotMatch should not contain extra properties', () => {
      // This test is helpful to be sure the doMatch/doNotMatch types remain correct during the IsFeatureEnabled -> useIsFeatureEnabled -> checkCompatibility chain
      // All the unit tests can be found above

      render(
        <StoreProvider>
          <IsFeatureEnabled
            // TODO: use a real feature
            feature="unitTestActiveEeTrialFake"
            ifDisabled={result => {
              // Types testing
              // @ts-expect-error The error is expected because here we are checking that the value reflects the runtime value
              expect(result.doNotMatch.ee?.withoutLicense).toBe(undefined);
              expect(result.doNotMatch.ee).not.toHaveProperty('withoutLicense');

              return <div>Disabled</div>;
            }}
          >
            <div>TODO:</div>
          </IsFeatureEnabled>
        </StoreProvider>
      );

      expect(screen.getByText('Disabled')).toBeInTheDocument();
    });
  });
});
