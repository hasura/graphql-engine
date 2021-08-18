import { testMode } from '../../../helpers/common';

import { inconsistentMetadataPage } from './spec';

export const testInconsistentMetadatapage = () => {
  describe('Inconsistent Metadata', () => {
    it(
      'should render inconsistent metadata table with fake data',
      inconsistentMetadataPage
    );
  });
};

if (testMode !== 'cli') {
  testInconsistentMetadatapage();
}
