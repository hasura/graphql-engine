import { testMode } from '../../../../helpers/common';
import {
  addSuccessfulInsecureDomain,
  deleteInsecureDomain,
  duplicateAndEmptyDomainError,
} from './spec';

export const testInsecureDomainPage = () => {
  describe('Insecure Domain', () => {
    it('should successfully add insecure domain', addSuccessfulInsecureDomain);
    it(
      'should show error on passing duplicate and empty domain name',
      duplicateAndEmptyDomainError
    );
    it('should delete domains one by one', deleteInsecureDomain);
  });
};

if (testMode !== 'cli') {
  testInsecureDomainPage();
}
