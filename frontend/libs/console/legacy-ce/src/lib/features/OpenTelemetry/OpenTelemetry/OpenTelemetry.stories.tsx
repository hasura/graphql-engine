import type { ComponentPropsWithoutRef } from 'react';
import type { ComponentStory, ComponentMeta } from '@storybook/react';

import * as React from 'react';
import { expect } from '@storybook/jest';
import { useEffect, useState } from 'react';
import { action } from '@storybook/addon-actions';
import { waitFor, within } from '@storybook/testing-library';

import { defaultValues as defaultFormValues } from './components/Form/schema';
import { OpenTelemetry } from './OpenTelemetry';

// --------------------------------------------------
// NOT TESTED
// --------------------------------------------------
// The whole component is not tested to avoid adding too much integration tests that bring little
// confidence value and a high number of tests to maintain. Due to the limited logics inside it, the
// component is tested through the parent component.

export default {
  title: 'Features/OpenTelemetry/OpenTelemetry',
  component: OpenTelemetry,
} as ComponentMeta<typeof OpenTelemetry>;

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
export const Disabled: ComponentStory<typeof OpenTelemetry> = args => {
  return <OpenTelemetry {...args} />;
};

Disabled.storyName = '💠 Disabled';

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const disabledArgs: ComponentPropsWithoutRef<typeof OpenTelemetry> = {
  isFirstTimeSetup: true,

  setOpenTelemetry: (...args) => {
    action('setOpenTelemetry')(...args);

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
    connectionType: 'http/protobuf',
  },
};
Disabled.args = disabledArgs;

// #endregion

// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// ENABLED STORY
// #region
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------
export const Enabled: ComponentStory<typeof OpenTelemetry> = args => {
  return <OpenTelemetry {...args} />;
};

Enabled.storyName = '💠 Enabled';

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const enabledArgs: ComponentPropsWithoutRef<typeof OpenTelemetry> = {
  isFirstTimeSetup: true,

  setOpenTelemetry: (...args) => {
    action('setOpenTelemetry')(...args);

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
    connectionType: 'http/protobuf',
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
export const Skeleton: ComponentStory<typeof OpenTelemetry> = args => {
  return <OpenTelemetry {...args} />;
};

Skeleton.storyName = '💠 Skeleton';

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const skeletonArgs: ComponentPropsWithoutRef<typeof OpenTelemetry> = {
  isFirstTimeSetup: true,

  setOpenTelemetry: (...args) => {
    action('updateOpenT')(...args);

    // Fake the server loading
    return new Promise(resolve => setTimeout(resolve, 1000));
  },

  skeletonMode: true,
  metadataFormValues: defaultFormValues,
};
Skeleton.args = skeletonArgs;

// #endregion

// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// DEFAULT VALUES TEST
// #region
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------
const loadingMetadataProps: ComponentPropsWithoutRef<typeof OpenTelemetry> = {
  skeletonMode: true,
  isFirstTimeSetup: false,
  setOpenTelemetry: () => Promise.resolve(),
  metadataFormValues: defaultFormValues,
};

const metadataLoadedProps: ComponentPropsWithoutRef<typeof OpenTelemetry> = {
  skeletonMode: false,
  isFirstTimeSetup: false,

  setOpenTelemetry: (...args) => {
    action('setOpenTelemetry')(...args);
    return Promise.resolve();
  },

  metadataFormValues: {
    // Using non-default values
    enabled: true,
    batchSize: 99,
    dataType: ['traces'],
    connectionType: 'http/protobuf',
    endpoint: 'http://localhost:1234',
    headers: [{ name: 'foo', value: 'bar', type: 'from_value' }],
    attributes: [{ name: 'foo', value: 'bar', type: 'from_value' }],
  },
};

export const DefaultValues: ComponentStory<never> = () => {
  // Initial placeholder props
  const [props, setProps] =
    useState<ComponentPropsWithoutRef<typeof OpenTelemetry>>(
      loadingMetadataProps
    );

  // Simulate passing from the loading to the default state
  useEffect(() => {
    const timeoutId = setTimeout(() => {
      setProps(metadataLoadedProps);
    }, 100);

    return () => clearTimeout(timeoutId);
  }, []);

  return <OpenTelemetry {...props} />;
};

DefaultValues.storyName =
  '🧪 Testing - When a configuration is available, must use it as the default values';

DefaultValues.parameters = {
  chromatic: { disableSnapshot: true },
};

// --------------------------------------------------
// INTERACTION TEST
// --------------------------------------------------
DefaultValues.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  // STEP: Ensure the props used for the test are different from the default ones
  expect(metadataLoadedProps.metadataFormValues).not.toEqual(defaultFormValues);

  // STEP: Wait until the metadata has been loaded (through waiting for the submit button being enabled)
  const submitButton = await canvas.findByRole('button', { name: 'Update' });
  await waitFor(() => {
    expect(submitButton).toBeEnabled();
  });

  // STEP: check the default value of the Batch Size
  const batchSizeInputField = await canvas.findByLabelText('Batch Size', {
    selector: 'input',
  });
  expect(batchSizeInputField).toHaveValue(99);

  // All the other input fields are not tested since if one input field has the correct default value
  // all of the other input fields have the correct default value.
};

// #endregion
