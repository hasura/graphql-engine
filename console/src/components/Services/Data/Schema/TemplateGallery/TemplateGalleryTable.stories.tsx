import { ComponentMeta } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { TemplateGalleryBody } from './TemplateGalleryTable';
import { ReduxDecorator } from '../../../../../storybook/decorators/redux-decorator';
import { networkStubs } from './__tests__/stubs/schemaSharingNetworkStubs';

export default {
  title: 'Template Gallery/TemplateGalleryBody',
  component: TemplateGalleryBody,
} as ComponentMeta<typeof TemplateGalleryBody>;

const getDecorators = () => {
  return [
    ReduxDecorator({
      metadata: {
        metadataObject: { version: 3 },
      },
    }),
  ];
};

// Doing this instead of spreading inside between the story to have a unique store per story
const getDefaultStoryData = () => ({
  args: {
    onModalOpen: action('onModalOpen'),
  },
  decorators: getDecorators(),
});

export const ActualData = {
  ...getDefaultStoryData(),
  parameters: {
    msw: [],
  },
};

export const FullData = {
  ...getDefaultStoryData(),
  parameters: {
    msw: [networkStubs.rootJson],
  },
};

export const EmptyData = {
  ...getDefaultStoryData(),
  parameters: {
    msw: [networkStubs.rootJsonEmpty],
  },
};

export const Errored = {
  ...getDefaultStoryData(),
  parameters: {
    msw: [networkStubs.rootJsonError],
  },
};

export const Loading = {
  ...getDefaultStoryData(),
  parameters: {
    msw: [networkStubs.rootJsonWithLoading],
  },
};
