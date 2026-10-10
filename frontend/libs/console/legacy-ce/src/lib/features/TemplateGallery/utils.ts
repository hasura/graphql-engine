import { SupportedDriver } from '@hasura/shared/types';
import { TemplateGallerySection } from './types';

export const getSchemasForDb = (
  templates: TemplateGallerySection[],
  driver: SupportedDriver,
) =>
  templates
    .map((section) => ({
      ...section,
      templates: section.templates.filter(
        (template) => template.dialect === driver,
      ),
    }))
    .filter((section) => section.templates.length > 0);

export const getTemplateBySectionAndKey = (
  sections: TemplateGallerySection[] | undefined,
  { key, section }: { key: string; section: string },
) => {
  if (!sections?.length) {
    return undefined;
  }

  const maybeSection = sections.find((block) => block.name === section);
  if (!maybeSection) {
    return undefined;
  }
  const maybeTemplate = maybeSection.templates.find(
    (template) => template.key === key,
  );
  if (!maybeTemplate) {
    return undefined;
  }

  return maybeTemplate;
};
