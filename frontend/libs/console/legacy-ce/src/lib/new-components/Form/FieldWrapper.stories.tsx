import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';

import { FieldWrapper } from './FieldWrapper';

const ChildrenExample = () => (
  <div
    className="text-black dark:text-white text-center bg-gray-200 dark:bg-gray-700 py-1 rounded"
    style={{ backgroundImage: '100%' }}
  >
    &lt;--SLOT--&gt;
  </div>
);

export default {
  title: 'components/Forms 📁/FieldWrapper 🧬',
  component: FieldWrapper,
  parameters: {
    docs: {
      description: {
        component: `A utility component wrapping all needed elements to build form fields: **label**, **description** and **error message**.
The wrapped field is added in a dedicated **slot** used to build form fields components usable in forms .

Default CSS display is \`block\`, provided without padding and margin (displayed here with the \`<SimpleForm>\` padding).`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [
    Story => <div className="p-4 flex w-full max-w-screen">{Story()}</div>,
  ],
} as ComponentMeta<typeof FieldWrapper>;

export const ApiPlayground: ComponentStory<typeof FieldWrapper> = args => (
  <FieldWrapper {...args} />
);
ApiPlayground.storyName = '⚙️ API';
ApiPlayground.args = {
  label: 'The field wrapper label',
  description: 'The field wrapper description',
  tooltip: 'The field wrapper tooltip',
  children: <ChildrenExample />,
};

export const Basic: ComponentStory<typeof FieldWrapper> = () => (
  <FieldWrapper label="The field wrapper label">
    <ChildrenExample />
  </FieldWrapper>
);
Basic.storyName = '🧰 Basic';
Basic.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantWithDescription: ComponentStory<
  typeof FieldWrapper
> = () => (
  <FieldWrapper
    label="The field wrapper label"
    description="The field wrapper description"
  >
    <ChildrenExample />
  </FieldWrapper>
);
VariantWithDescription.storyName = '🎭 Variant - With description';
VariantWithDescription.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantWithTooltip: ComponentStory<typeof FieldWrapper> = () => (
  <FieldWrapper
    label="The field wrapper label"
    tooltip="The field wrapper tooltip"
  >
    <ChildrenExample />
  </FieldWrapper>
);
VariantWithTooltip.storyName = '🎭 Variant - With tooltip';
VariantWithTooltip.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantWithDescriptionAndTooltip: ComponentStory<
  typeof FieldWrapper
> = () => (
  <FieldWrapper
    label="The field wrapper label"
    description="The field wrapper description"
    tooltip="The field wrapper tooltip"
  >
    <ChildrenExample />
  </FieldWrapper>
);
VariantWithDescriptionAndTooltip.storyName =
  '🎭 Variant - With description and tooltip';
VariantWithDescriptionAndTooltip.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantWithDescriptionAndTooltipAndKnwMoreLink: ComponentStory<
  typeof FieldWrapper
> = () => (
  <FieldWrapper
    label="The field wrapper label"
    description="The field wrapper description"
    tooltip="The field wrapper tooltip"
    learnMoreLink="https://hasura.io/docs"
  >
    <ChildrenExample />
  </FieldWrapper>
);
VariantWithDescriptionAndTooltipAndKnwMoreLink.storyName =
  '🎭 Variant - With description, tooltip, and know more link';
VariantWithDescriptionAndTooltipAndKnwMoreLink.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantWithDescriptionAndTooltipAndLearnMoreLink: ComponentStory<
  typeof FieldWrapper
> = () => (
  <FieldWrapper
    label="The field wrapper label"
    description="The field wrapper description"
    tooltip="The field wrapper tooltip"
    learnMoreLink="https://hasura.io/docs"
  >
    <ChildrenExample />
  </FieldWrapper>
);
VariantWithDescriptionAndTooltipAndLearnMoreLink.storyName =
  '🎭 Variant - With description, tooltip, and know more link';
VariantWithDescriptionAndTooltipAndLearnMoreLink.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const StateLoading: ComponentStory<typeof FieldWrapper> = () => (
  <FieldWrapper
    label="The field wrapper label"
    description="The field wrapper description"
    tooltip="The field wrapper tooltip"
    loading
  >
    <ChildrenExample />
  </FieldWrapper>
);
StateLoading.storyName = '🔁 State - Loading';
StateLoading.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const StateWithErrorMessage: ComponentStory<
  typeof FieldWrapper
> = () => (
  <FieldWrapper
    label="The field wrapper label"
    description="The field wrapper description"
    tooltip="The field wrapper tooltip"
    error={{ message: 'The error message', type: 'error' }}
  >
    <ChildrenExample />
  </FieldWrapper>
);
StateWithErrorMessage.storyName = '🔁 State - With error message';
StateWithErrorMessage.parameters = {
  docs: {
    description: {
      story: `Incorrect value is set then \`<SimpleForm>\` validation is automatically triggered.`,
    },
    source: { state: 'open' },
  },
};

export const TestingScalability: ComponentStory<typeof FieldWrapper> = () => (
  <FieldWrapper
    label="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    description="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    tooltip="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    error={{
      message:
        'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.',
      type: 'error',
    }}
  >
    <ChildrenExample />
  </FieldWrapper>
);
TestingScalability.storyName = '🧪 Testing - Scalability';
TestingScalability.parameters = {
  docs: {
    source: { state: 'open' },
  },
};
