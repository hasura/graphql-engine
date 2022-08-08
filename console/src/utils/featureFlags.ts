type Status = 'enabled' | 'disabled';

export const ENABLE_AUTH_LAYER = true;

/*
This enables the developement code of the GDC tree view on the console to become active. 
Once enabled, the TreeView.tsx will render a tree nav for non-native DBs (i.e DBs that are no pg, citus, mssql and bq) in the Data Tab.
This feature is under a development flag because the API and the metadata structure is not decided yet.
*/
export const GDC_TREE_VIEW_DEV: Status = 'disabled';
