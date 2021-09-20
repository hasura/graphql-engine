import { ComponentMeta } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { ReduxDecorator } from '../../../../../storybook/decorators/redux-decorator';
import { TemplateGalleryModal } from './TemplateGalleryModal';
import { networkStubs } from './__tests__/stubs/schemaSharingNetworkStubs';

export default {
  title: 'Template Gallery/TemplateGalleryModal',
  component: TemplateGalleryModal,
} as ComponentMeta<typeof TemplateGalleryModal>;

const getDecorators = () => {
  return [
    ReduxDecorator({
      metadata: {
        metadataObject: { version: 3 },
      },
      templateGallery: {
        templates: {
          sections: [
            {
              name: 'AAAAAAAA',
              templates: [
                {
                  type: 'database',
                  isPartialData: true,
                  fetchingStatus: 'none',
                  key: 'template-1',
                  description: 'This is the description of template one',
                  dialect: 'postgres',
                  title: 'template-1',
                  relativeFolderPath: './postgres-template-1',
                  metadataVersion: 3,
                  templateVersion: 1,
                },
              ],
            },
          ],
        },
      },
    }),
  ];
};

// Doing this instead of spreading inside between the story to have a unique store per story
const getDefaultStoryData = () => ({
  args: {
    content: { key: 'template-1', section: 'AAAAAAAA' },
    closeModal: action('closeModal'),
  },
  decorators: getDecorators(),
});

export const FullData = {
  ...getDefaultStoryData(),
  parameters: {
    msw: [networkStubs.rootJson],
  },
};
