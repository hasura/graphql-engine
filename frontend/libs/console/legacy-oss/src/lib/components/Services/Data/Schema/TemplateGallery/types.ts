import { HasuraMetadataV3 } from '../../../../../metadata/types';
import { Driver } from '../../../../../dataSources';

export type ModalType = { key: string; section: string };

export type modalOpenFn = (params: ModalType) => void;

export type TemplateGalleryFetchingStatus =
  | 'success'
  | 'fetching'
  | 'failure'
  | 'none';

export type TemplateGalleryTemplateDetailFull = {
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

export type TemplateGalleryTemplateItem = {
  templateVersion: number;
  metadataVersion: number;
  key: string;
  type: 'database';
  title: string;
  description: string;
  relativeFolderPath: string;
  dialect: Driver;
  fetchingStatus: TemplateGalleryFetchingStatus;
  isPartialData: boolean;
  details?: TemplateGalleryTemplateDetailFull;
};

export interface TemplateGallerySection {
  name: string;
  templates: TemplateGalleryTemplateItem[];
}

export interface TemplateGalleryStore {
  globalConfigState: TemplateGalleryFetchingStatus;
  templates?: {
    sections: TemplateGallerySection[];
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

export interface ServerJsonTemplateDefinition {
  longDescription?: string;
  imageUrl?: string;
  blogPostLink?: string;
  sqlFiles: string[];
  metadataUrl?: string;
}
