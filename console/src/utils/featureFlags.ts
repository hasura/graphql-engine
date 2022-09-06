type Status = 'enabled' | 'disabled';

export const ENABLE_AUTH_LAYER = true;

/*
This enables the development code of the GDC connect database form on the console to become active. 
Once enabled, the form on the connect db page will be dynamically rendered based on the selected database type.
This feature is under a development flag because the API and the metadata structure is not decided yet.
*/
export const GDC_DB_CONNECTOR_DEV: Status = 'enabled';
