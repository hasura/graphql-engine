import React from 'react';
import clsx from 'clsx';
import { IconTooltip } from '../../../Tooltip';
import { LearnMoreLink } from '../../../link/LearnMoreLink';
import { Flex, Skeleton } from '@radix-ui/themes';
import { Text } from '../../../typography';
import { IconType } from 'react-icons';

export type FieldLabelProps = React.ComponentProps<'div'> & {
  /**
   * The field description
   */
  description?: string;
  /**
   * The field label
   */
  label: React.ReactNode;
  /**
   * The field label icon. Can be set only if label is set.
   */
  labelIcon?: IconType | React.ReactElement<any>;
  /**
   * The field tooltip label. Can be set only if label is set.
   */
  tooltip?: React.ReactNode;
  /**
   * The link containing more information about the field. Can be set only if label is set.
   */
  learnMoreLink?: string;
  /**
   * The custom text for the learn more link. Can be set only if label is set.
   */
  learnMoreLinkText?: string;
  /**
   * Flag indicating whether the field is loading
   */
  loading?: boolean;
  /**
   * Render line breaks in the description
   */
  renderDescriptionLineBreaks?: boolean;
  /**
   * tooltip icon other then ?
   */
  tooltipIcon?: React.ReactElement<any>;
};

export const FieldLabel = ({
  id,
  labelIcon: LabelIcon,
  label,
  learnMoreLink,
  learnMoreLinkText,
  className,
  tooltipIcon,
  description,
  tooltip,
  loading = false,
  renderDescriptionLineBreaks = false,
  ...rest
}: FieldLabelProps) => {
  const fieldLabelIcon = () => {
    if (!LabelIcon) {
      return null;
    }

    return typeof LabelIcon === 'function' ? (
      <LabelIcon className="h-4 w-4" />
    ) : (
      LabelIcon
    );
  };

  const content = (
    <>
      <Flex align="center" gap="1" {...rest}>
        <Skeleton loading={loading}>
          <Flex gap="2" align="center">
            {fieldLabelIcon()}
            {typeof label === 'string' ? (
              <Text weight="medium">{label}</Text>
            ) : (
              label
            )}
          </Flex>
        </Skeleton>
        {!loading && tooltip ? (
          <IconTooltip message={tooltip} icon={tooltipIcon} />
        ) : null}
        {!loading && !!learnMoreLink && (
          <LearnMoreLink href={learnMoreLink} text={learnMoreLinkText} />
        )}
      </Flex>
      {description ? (
        <Skeleton loading={loading}>
          <Text
            color="gray"
            size="1"
            className={clsx(
              renderDescriptionLineBreaks && 'whitespace-pre-line',
            )}
          >
            {description}
          </Text>
        </Skeleton>
      ) : null}
    </>
  );
  return id ? (
    <div className={className}>
      <label htmlFor={id}>{content}</label>
    </div>
  ) : (
    <div className={className}>{content}</div>
  );
};
