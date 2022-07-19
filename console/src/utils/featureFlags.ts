type Status = 'enabled' | 'disabled';

export const ENABLE_AUTH_LAYER = true;

/*
This enables the developement code of the GDC tree view on the console to become active. 
Once enabled, the TreeView.tsx will render a hardcoded GDC sample database with 3 levels in it, which is visible from the Data Tab.
This feature is under a development flag because the API and the metadata structure is not decided yet and acts more of a POC.
*/
export const GDC_TREE_VIEW_DEV: Status = 'disabled';
