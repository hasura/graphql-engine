import type { Meta, StoryObj } from '@storybook/react';
import type { ComponentPropsWithoutRef } from 'react';

import { action } from '@storybook/addon-actions';
import { expect } from '@storybook/jest';
import { waitFor, within } from '@storybook/testing-library';
import { useEffect, useState } from 'react';

import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { OpenTelemetry } from './OpenTelemetry';
import { defaultValues as defaultFormValues } from './components/Form/schema';

// --------------------------------------------------
// NOT TESTED
// --------------------------------------------------
// The whole component is not tested to avoid adding too much integration tests that bring little
// confidence value and a high number of tests to maintain. Due to the limited logics inside it, the
// component is tested through the parent component.

export default {
  title: 'Features/OpenTelemetry/OpenTelemetry',
  component: OpenTelemetry,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof OpenTelemetry>;

export const Disabled: StoryObj<typeof OpenTelemetry> = {
  render: args => {
    return <OpenTelemetry {...args} />;
  },

  name: '💠 Disabled',
};

export const DisabledWithoutLicense: StoryObj<typeof OpenTelemetry> = {
  render: args => {
    return <OpenTelemetry {...args} />;
  },

  name: '💠 Disabled without license',
  args: {
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
      tracesEndpoint: '',
      metricsEndpoint: '',
      logsEndpoint: '',
      batchSize: 512,
      attributes: [],
      dataType: ['traces'],
      connectionType: 'http/protobuf',
      tracesPropagators: [],
    },

    withoutLicense: true,
  },
};

export const Enabled: StoryObj<typeof OpenTelemetry> = {
  render: args => {
    return <OpenTelemetry {...args} />;
  },

  name: '💠 Enabled',
  args: {
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
      tracesEndpoint: '',
      metricsEndpoint: '',
      logsEndpoint: '',
      batchSize: 512,
      attributes: [],
      dataType: ['traces'],
      connectionType: 'http/protobuf',
      tracesPropagators: [],
    },
  },
};

export const Skeleton: StoryObj<typeof OpenTelemetry> = {
  render: args => {
    return <OpenTelemetry {...args} />;
  },

  name: '💠 Skeleton',
  args: {
    isFirstTimeSetup: true,

    setOpenTelemetry: (...args) => {
      action('updateOpenT')(...args);

      // Fake the server loading
      return new Promise(resolve => setTimeout(resolve, 1000));
    },

    skeletonMode: true,
    metadataFormValues: defaultFormValues,
  },
};

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
  eeAccess: 'active',
  isFirstTimeSetup: false,
  setOpenTelemetry: () => Promise.resolve(),
  metadataFormValues: defaultFormValues,
};

const metadataLoadedProps: ComponentPropsWithoutRef<typeof OpenTelemetry> = {
  eeAccess: 'active',
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
    dataType: ['traces', 'metrics', 'logs'],
    connectionType: 'http/protobuf',
    logsEndpoint: 'http://localhost:1234',
    tracesEndpoint: 'http://localhost:1234',
    metricsEndpoint: 'http://localhost:1234',
    tracesPropagators: ['tracecontext'],
    headers: [{ name: 'foo', value: 'bar', type: 'from_value' }],
    attributes: [{ name: 'foo', value: 'bar', type: 'from_value' }],
  },
};

export const DefaultValues: StoryObj = {
  render: () => {
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
  },

  name: '🧪 Testing - When a configuration is available, must use it as the default values',

  parameters: {
    chromatic: { disableSnapshot: true },
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    // STEP: Ensure the props used for the test are different from the default ones
    expect(metadataLoadedProps.metadataFormValues).not.toEqual(
      defaultFormValues
    );

    // STEP: Wait until the metadata has been loaded (through waiting for the submit button being enabled)
    const submitButton = await canvas.findByRole('button', { name: 'Update' });
    await waitFor(() => {
      expect(submitButton).toBeEnabled();
    });

    // STEP: check the default value of the Batch Size
    const batchSizeInputField = await canvas.findByLabelText('Batch Size', {
      selector: 'input',
    });

    // STEP: check the value of the Traces Endpoint
    const tracesEndpoint = await canvas.findByLabelText('Traces Endpoint', {
      selector: 'input',
    });

    // STEP: check the value of the Metrics Endpoint
    const metricsEndpoint = await canvas.findByLabelText('Metrics Endpoint', {
      selector: 'input',
    });

    // STEP: check the value of the Logs Endpoint
    const logsEndpoint = await canvas.findByLabelText('Logs Endpoint', {
      selector: 'input',
    });

    expect(batchSizeInputField).toHaveValue(99);
    expect(tracesEndpoint).toHaveValue('http://localhost:1234');
    expect(metricsEndpoint).toHaveValue('http://localhost:1234');
    expect(logsEndpoint).toHaveValue('http://localhost:1234');

    // All the other input fields are not tested since if one input field has the correct default value
    // all of the other input fields have the correct default value.
  },
};
