import { HasuraMetadataV3 } from '../../../../../metadata/types';
import { Driver } from '../../../../../dataSources';

export type ModalType = { key: string; section: string };

export type modalOpenFn = (params: ModalType) => void;

export type SchemaSharingFetchingStatus =
  | 'success'
  | 'fetching'
  | 'failure'
  | 'none';

export type SchemaSharingTemplateDetailFull = {
  sql: string;
  longDescription?: string;
  imageUrl?: string;
  publicUrl: string;
  blogPostLink?: string;
  metadataObject?: {
    resource_version: number;
    metadata: HasuraMetadataV3;
  };
};

export type SchemaSharingTemplateItem = {
  templateVersion: number;
  metadataVersion: number;
  key: string;
  type: 'database';
  title: string;
  description: string;
  relativeFolderPath: string;
  dialect: Driver;
  fetchingStatus: SchemaSharingFetchingStatus;
  isPartialData: boolean;
  details?: SchemaSharingTemplateDetailFull;
};

export interface SchemaSharingSection {
  name: string;
  templates: SchemaSharingTemplateItem[];
}

export interface SchemaSharingStore {
  globalConfigState: SchemaSharingFetchingStatus;
  schemas?: {
    sections: SchemaSharingSection[];
  };
}

export interface ServerJsonRootConfig {
  [key: string]: {
    template_version: string;
    metadata_version: string;
    type: 'database';
    dialect: Driver;
    title: string;
    description: string;
    relativeFolderPath: string;
    category: string;
  };
}

export interface ServerJsonSchemaDefinition {
  longDescription?: string;
  imageUrl?: string;
  blogPostLink?: string;
  sqlFiles: string[];
  metadataUrl?: string;
}
