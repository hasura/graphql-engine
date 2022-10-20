import { parseEventsSQLResp } from '../utils';
import events from './fixtures/events.json';
import invocations from './fixtures/invocations.json';

describe('Events_Utils.ts', () => {
  describe('parseEventsSQLResp', () => {
    it('should parse events', () => {
      const parsedEvents = parseEventsSQLResp(events);
      expect(parsedEvents.length).toBe(5);
      expect(parsedEvents?.[0]?.error).toBe(false);
      expect(parsedEvents?.[0]?.archived).toBe(false);
      expect(parsedEvents?.[0]?.delivered).toBe(false);
      expect(parsedEvents).toMatchSnapshot();
    });

    it('should parse event invocations', () => {
      const parsedInvocations = parseEventsSQLResp(invocations);
      expect(parsedInvocations?.[0]?.error).toBe(true);
      expect(parsedInvocations?.[0]?.archived).toBe(false);
      expect(parsedInvocations?.[0]?.delivered).toBe(false);
      expect(typeof parsedInvocations?.[0]?.request).toBe('object');
      expect(parsedInvocations.length).toBe(10);
      expect(parsedInvocations).toMatchSnapshot();
    });
  });
});
