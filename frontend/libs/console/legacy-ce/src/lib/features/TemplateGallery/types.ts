import { HasuraMetadataV3, SupportedDriver } from '@hasura/shared/types';

const repo_owner = 'hasura';
const repo_name = 'template-gallery';
const repo_branch = 'main';

export const BASE_URL_TEMPLATE = `https://raw.githubusercontent.com/${repo_owner}/${repo_name}/${repo_branch}`;
export const BASE_URL_PUBLIC = `https://github.com/${repo_owner}/${repo_name}/blob/${repo_branch}`;
export const ROOT_CONFIG_PATH = `${BASE_URL_TEMPLATE}/index.json`;

export type modalOpenFn = (params: TemplateGalleryTemplateItem) => void;

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
  dialect: SupportedDriver;
};

export interface TemplateGallerySection {
  name: string;
  templates: TemplateGalleryTemplateItem[];
}

export interface TemplateGalleryStore {
  templates?: {
    sections: TemplateGallerySection[];
  };
}

export interface ServerJsonRootConfig {
  [key: string]: {
    template_version: string;
    metadata_version: string;
    type: 'database';
    dialect: SupportedDriver;
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
