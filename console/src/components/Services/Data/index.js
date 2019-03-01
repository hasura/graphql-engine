/**
 *  Point of contact for component modules
 *
 *  ie: import { CounterButton, InfoBar } from 'components';
 *
 */

export dataHeaderConnector from './DataHeader';
export PageContainer from './PageContainer/PageContainer';
export viewTableConnector from './TableBrowseRows/ViewTable';
export addExistingTableViewConnector from './Add/AddExistingTableView';
export addTableConnector from './Add/AddTable';
export rawSQLConnector from './RawSQL/RawSQL';
export insertItemConnector from './TableInsertItem/InsertItem';
export editItemConnector from './TableBrowseRows/EditItem';
export modifyTableConnector from './TableModify/ModifyTable';
export modifyViewConnector from './TableModify/ModifyView';
export relationshipsConnector from './TableRelationships/Relationships';
export relationshipsViewConnector from './TableRelationships/RelationshipsView';
export permissionsConnector from './TablePermissions/Permissions';
export schemaConnector from './Schema/Schema';
export schemaContainerConnector from './Schema/SchemaContainer';
export migrationsConnector from './Migrations/MigrationsHome';

export dataRouter from './DataRouter';

export dataReducer from './DataReducer';

export metadataConnector from './Metadata/Metadata.js';

/* Function component */

export functionWrapperConnector from './Function/FunctionWrapper';
export ModifyCustomFunction from './Function/Modify/ModifyCustomFunction';
export PermissionCustomFunction from './Function/Permission/Permission';

/*
export Logs from './Logs/Logs';
export BrowseTemplates from './QueryTemplates/BrowseTemplates';
export CreateTemplate from './QueryTemplates/CreateTemplate';
export ListTemplates from './QueryTemplates/ListTemplates';
export ViewTemplate from './QueryTemplates/ViewTemplate';
*/
