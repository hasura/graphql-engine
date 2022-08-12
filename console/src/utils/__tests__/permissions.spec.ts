import * as proConsole from '../proConsole';
import {
  canAccessReadReplica,
  canAccessSecuritySettings,
} from '../permissions';

jest.mock('../proConsole', () => ({
  isProConsole: jest.fn(() => true),
}));

const mockedIsProConsole = jest.spyOn(proConsole, 'isProConsole');

describe('canAccessReadReplica', () => {
  it('returns true on pro console', () => {
    mockedIsProConsole.mockImplementation(() => true);
    expect(canAccessReadReplica()).toBe(true);
  });

  it('returns false if console is NOT pro', () => {
    mockedIsProConsole.mockImplementation(() => false);
    expect(canAccessReadReplica()).toBe(false);
  });
});

describe('canAccessSecuritySettings', () => {
  it('returns true on pro console', () => {
    mockedIsProConsole.mockImplementation(() => true);
    expect(canAccessSecuritySettings()).toBe(true);
  });

  it('returns false if console is NOT pro', () => {
    mockedIsProConsole.mockImplementation(() => false);
    expect(canAccessSecuritySettings()).toBe(false);
  });
});
