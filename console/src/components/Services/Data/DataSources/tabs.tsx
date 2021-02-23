import { Tabs } from '../../../Common/Layout/ReusableTabs/ReusableTabs';
import Globals from '../../../../Globals';

const tabs: Tabs = {
  connect: {
    display_text: 'Connect existing database',
  },
};
if (Globals.hasuraCloudTenantId && Globals.herokuOAuthClientId) {
  tabs.create = {
    display_text: 'Create Heroku Database',
  };
}

export default tabs;
