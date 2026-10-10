import { getWebsocketProtocol } from './protocol';

describe('getWebsocketProtocol', () => {
  const testCases = [
    {
      name: 'when invalid url is passed, returns default (ws) ',
      input: '',
      output: 'ws:',
    },
    {
      name: 'when http url is passed, returns ws',
      input: 'http:',
      output: 'ws:',
    },
    {
      name: 'when https url is passed, returns wss',
      input: 'https:',
      output: 'wss:',
    },
  ];
  testCases.forEach(({ input, output, name }) => {
    it(name, () => {
      expect(getWebsocketProtocol(input)).toEqual(output);
    });
  });
});
