import React from 'react';
import { FaGithub, FaUpload } from 'react-icons/fa';
import styles from './TemplateGallery.module.scss';
import {
  TemplateGalleryTemplateDetailFull,
  TemplateGalleryTemplateItem,
} from './types';
import { useSchemaConfigurationByName } from './hooks/useSchemaConfigurationByName';
import useApplyTemplate from './hooks/useApplyTemplate';
import { Source } from '@hasura/shared/types';
import { Link } from '@radix-ui/themes';
import {
  Button,
  Dialog,
  SkeletonList,
  IndicatorCard,
  Text,
  Separator,
  SqlCodeBlock,
} from '@hasura/shared/ui';

export const TemplateGalleryModalBody: React.FC<{
  template: TemplateGalleryTemplateDetailFull | undefined;
  isFetching: boolean;
  error?: unknown;
}> = ({ template, isFetching, error }) => {
  if (isFetching) {
    return <SkeletonList count={5} />;
  }

  if (!template || error) {
    return (
      <IndicatorCard status="negative" showIcon>
        Something went wrong, please try again later.
      </IndicatorCard>
    );
  }

  return (
    <div>
      {template.longDescription ? (
        <Text as="p">{template.longDescription}</Text>
      ) : null}
      {template.blogPostLink ? (
        <div className="mt-2">
          <Text>
            Read the blog post{' '}
            <Link
              href={template.blogPostLink}
              target="_blank"
              rel="noopener noreferrer"
            >
              here
            </Link>
            .
          </Text>
          <Separator size="4" className="my-4" />
        </div>
      ) : null}
      {template.imageUrl ? (
        <p className="py-3">
          <img
            className={styles.image_in_detail}
            src={template.imageUrl}
            alt=""
          />
        </p>
      ) : null}
      <Text weight="medium">SQL:</Text>
      <SqlCodeBlock text={template.sql} />
    </div>
  );
};

export const TemplateGalleryModal: React.FC<{
  source: Source;
  currentTemplate: TemplateGalleryTemplateItem;
  closeModal: () => void;
}> = ({ currentTemplate, source, closeModal }) => {
  const {
    data: templateDetail,
    isFetching,
    error,
  } = useSchemaConfigurationByName({
    template: currentTemplate,
  });
  const { mutate: applyTemplate, isPending: isApplying } = useApplyTemplate();

  if (currentTemplate === undefined) {
    return <div />;
  }

  const onSubmit = () => {
    if (!templateDetail) {
      return;
    }

    applyTemplate(
      {
        details: templateDetail,
        source,
        template: currentTemplate,
      },
      {
        onSuccess: () => {
          closeModal();
        },
      },
    );
  };

  const shouldDisplaySubmit = Boolean(currentTemplate);

  return (
    <Dialog
      title={currentTemplate.title}
      size="xxxl"
      onClose={closeModal}
      footer={
        shouldDisplaySubmit
          ? {
              callToAction: 'Install Template',
              callToActionProps: {
                leftIcon: FaUpload,
              },
              isLoading: isApplying,
              onSubmit,
              leftContent: (
                <Link
                  asChild
                  href={templateDetail?.publicUrl}
                  underline="none"
                  target="_blank"
                >
                  <Button
                    mode="default"
                    leftIcon={FaGithub}
                    disabled={isApplying}
                  >
                    View on GitHub
                  </Button>
                </Link>
              ),
            }
          : undefined
      }
    >
      <TemplateGalleryModalBody
        template={templateDetail}
        isFetching={isFetching}
        error={error}
      />
    </Dialog>
  );
};
