import { FeatureFlagDefinition, FeatureFlagState } from '../../types';
import { mergeFlagWithState } from '../useFeatureFlags';

describe('mergeFlagWithState', () => {
  it('should merge known data', () => {
    const inputFlags: FeatureFlagDefinition[] = [
      {
        id: 'AAA',
        defaultValue: false,
        description: 'DESCR',
        discussionUrl: '',
        section: 'api',
        status: 'beta',
        title: 'Hello world',
      },
    ];
    const inputState: FeatureFlagState[] = [
      { id: 'AAA', dismissed: false, enabled: true },
    ];

    const result = mergeFlagWithState(inputFlags, inputState);

    expect(result).toMatchInlineSnapshot(`
      [
        {
          "defaultValue": false,
          "description": "DESCR",
          "discussionUrl": "",
          "id": "AAA",
          "section": "api",
          "state": {
            "dismissed": false,
            "enabled": true,
            "id": "AAA",
          },
          "status": "beta",
          "title": "Hello world",
        },
      ]
    `);
  });

  it('should ignore non existing flags', () => {
    const inputFlags: FeatureFlagDefinition[] = [
      {
        id: 'AAA',
        defaultValue: false,
        description: 'DESCR',
        discussionUrl: '',
        section: 'api',
        status: 'beta',
        title: 'Hello world',
      },
    ];
    const inputState: FeatureFlagState[] = [
      { id: 'CCC', dismissed: false, enabled: true },
    ];

    const result = mergeFlagWithState(inputFlags, inputState);

    expect(result).toMatchInlineSnapshot(`
      [
        {
          "defaultValue": false,
          "description": "DESCR",
          "discussionUrl": "",
          "id": "AAA",
          "section": "api",
          "state": {
            "dismissed": false,
            "enabled": false,
          },
          "status": "beta",
          "title": "Hello world",
        },
      ]
    `);
  });

  it('should set the default value when none is present', () => {
    const inputFlags: FeatureFlagDefinition[] = [
      {
        id: 'ON',
        defaultValue: true,
        description: 'DESCR',
        discussionUrl: '',
        section: 'api',
        status: 'beta',
        title: 'Hello world',
      },
      {
        id: 'off',
        defaultValue: false,
        description: 'DESCR',
        discussionUrl: '',
        section: 'api',
        status: 'beta',
        title: 'Hello world',
      },
    ];
    const inputState: FeatureFlagState[] = [];

    const result = mergeFlagWithState(inputFlags, inputState);

    expect(result).toMatchInlineSnapshot(`
      [
        {
          "defaultValue": true,
          "description": "DESCR",
          "discussionUrl": "",
          "id": "ON",
          "section": "api",
          "state": {
            "dismissed": false,
            "enabled": true,
          },
          "status": "beta",
          "title": "Hello world",
        },
        {
          "defaultValue": false,
          "description": "DESCR",
          "discussionUrl": "",
          "id": "off",
          "section": "api",
          "state": {
            "dismissed": false,
            "enabled": false,
          },
          "status": "beta",
          "title": "Hello world",
        },
      ]
    `);
  });
});
