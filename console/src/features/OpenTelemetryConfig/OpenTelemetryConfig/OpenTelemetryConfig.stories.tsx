import type { ComponentPropsWithoutRef } from 'react';
import type { ComponentStory, ComponentMeta } from '@storybook/react';

import * as React from 'react';
import { action } from '@storybook/addon-actions';

import { OpenTelemetryConfig } from './OpenTelemetryConfig';

// --------------------------------------------------
// NOT TESTED
// --------------------------------------------------
// The whole component is not tested to avoid adding too much integration tests that bring little
// confidence value and a high number of tests to maintain. Due to the limited logics inside it, the
// component is tested through the parent component.

export default {
  title: 'Features/OpenTelemetryConfig/OpenTelemetryConfig',
  component: OpenTelemetryConfig,
  argTypes: {
    updateOpenTelemetryConfig: { action: true },
  },
} as ComponentMeta<typeof OpenTelemetryConfig>;

// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// DISABLED STORY
// #region
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------
export const Disabled: ComponentStory<typeof OpenTelemetryConfig> = args => {
  return <OpenTelemetryConfig {...args} />;
};

Disabled.storyName = '💠 Disabled';

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const disabledArgs: ComponentPropsWithoutRef<typeof OpenTelemetryConfig> = {
  updateOpenTelemetryConfig: (...args) => {
    action('updateOpenTelemetryConfig')(...args);

    // Fake the server loading
    return new Promise(resolve => setTimeout(resolve, 1000));
  },

  skeletonMode: false,
  metadataFormValues: {
    enabled: false,

    headers: [],
    endpoint: '',
    batchSize: 512,
    attributes: [],
    dataType: ['traces'],
    connectionType: 'http',
  },
};
Disabled.args = disabledArgs;

// #endregion

// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// DISABLED STORY
// #region
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------
export const Enabled: ComponentStory<typeof OpenTelemetryConfig> = args => {
  return <OpenTelemetryConfig {...args} />;
};

Enabled.storyName = '💠 Enabled';

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const enabledArgs: ComponentPropsWithoutRef<typeof OpenTelemetryConfig> = {
  updateOpenTelemetryConfig: (...args) => {
    action('updateOpenTelemetryConfig')(...args);

    // Fake the server loading
    return new Promise(resolve => setTimeout(resolve, 1000));
  },

  skeletonMode: false,
  metadataFormValues: {
    enabled: true,

    headers: [],
    endpoint: '',
    batchSize: 512,
    attributes: [],
    dataType: ['traces'],
    connectionType: 'http',
  },
};
Enabled.args = enabledArgs;

// #endregion

// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// SKELETON STORY
// #region
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------
export const Skeleton: ComponentStory<typeof OpenTelemetryConfig> = args => {
  return <OpenTelemetryConfig {...args} />;
};

Skeleton.storyName = '💠 Skeleton';

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const skeletonArgs: ComponentPropsWithoutRef<typeof OpenTelemetryConfig> = {
  updateOpenTelemetryConfig: (...args) => {
    action('updateOpenT')(...args);

    // Fake the server loading
    return new Promise(resolve => setTimeout(resolve, 1000));
  },

  skeletonMode: true,
  metadataFormValues: undefined,
};
Skeleton.args = skeletonArgs;

// #endregion
