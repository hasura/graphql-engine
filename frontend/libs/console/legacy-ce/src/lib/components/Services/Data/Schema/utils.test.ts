import { checkHighLatencySources, getSourceInfoFromLatencyData } from './utils';
import { testDataOne, testDataTwo } from './utils.test.fixtures';

describe('checkHighLatencySources', () => {
  it('should report that there are some sources with high latency', () => {
    expect(checkHighLatencySources(testDataOne)).toBe(false);
  });
  it('should report that there are no sources with high latency', () => {
    expect(checkHighLatencySources(testDataTwo)).toBe(true);
  });
  it('should return `false` for input that is `undefined`', () => {
    expect(checkHighLatencySources(undefined)).toBe(false);
  });
});

describe('getSourceInfoFromLatencyData', () => {
  it('should return the latency data for source with name `default`', () => {
    expect(getSourceInfoFromLatencyData('default', testDataOne)).toBeTruthy();
  });
  it('should return the latency data for source with name `default2`', () => {
    expect(getSourceInfoFromLatencyData('default', testDataOne)).toBeTruthy();
  });
  it('should return `undefined` for sourceName that is `neon_db`', () => {
    expect(
      getSourceInfoFromLatencyData('neon_db', testDataOne)
    ).not.toBeTruthy();
  });
});
