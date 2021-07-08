export type ModalType = { key: string; section: string };

export type modalOpenFn = (params: ModalType) => void;

export type SchemaSharingFetchingStatus =
  | 'success'
  | 'fetching'
  | 'failure'
  | 'none';
