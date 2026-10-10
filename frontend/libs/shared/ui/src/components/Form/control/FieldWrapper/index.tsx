import * as React from 'react';
import { FieldError } from 'react-hook-form';
import clsx from 'clsx';
import { Flex, Skeleton } from '@radix-ui/themes';
import { ErrorMessage } from './ErrorMessage';
import { IconType } from 'react-icons';
import { FieldLabel } from './FieldLabel';

export * from './FieldLabel';
export * from './ErrorMessage';

export type FieldWrapperPassThroughProps = {
  /**
   * The field ID
   */
  id?: string;
  /**
   * The field size (full: the full width of the container , medium: half the
   * width of the container)
   */
  size?: 'full' | 'medium';
  /**
   * The field description
   */
  description?: string;
  /**
   * The field data test id for testing
   */
  dataTest?: string;
  /**
   * The field data test id for testing
   */
  dataTestId?: string;
  /**
   * Flag indicating whether the field is loading
   */
  loading?: boolean;
  /**
   * Removing styling only necessary for the error placeholder
   */
  noErrorPlaceholder?: boolean;
  /**
   * Render line breaks in the description
   */
  renderDescriptionLineBreaks?: boolean;
  /**
   * tooltip icon other then ?
   */
  tooltipIcon?: React.ReactElement<any>;

  /**
   * The field label
   */
  label?: React.ReactNode;
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
   * The orientation of label
   */
  orientation?: 'vertical' | 'horizontal';
};

type FieldWrapperProps = FieldWrapperPassThroughProps & {
  /**
   * The field class
   */
  className?: string;
  /**
   * The field children
   */
  children: React.ReactNode | (() => React.ReactNode);
  /**
   * The field error
   */
  error?: FieldError | undefined;
  /**
   * Disable wrapping children in layers of divs to enable impacting children with styles (e.g. centering a switch element)
   */
  doNotWrapChildren?: boolean;
};

export const FieldWrapper = (props: FieldWrapperProps) => {
  const {
    id,
    labelIcon,
    label,
    learnMoreLink,
    learnMoreLinkText,
    className,
    size = 'full',
    error,
    tooltipIcon,
    children,
    description,
    tooltip,
    orientation,
    loading = false,
    noErrorPlaceholder = false,
    renderDescriptionLineBreaks,
    doNotWrapChildren = false,
  } = props;

  const fieldErrors = (
    <div className="w-full">
      {loading ? (
        <Skeleton width="100%" height="30px" />
      ) : typeof children === 'function' ? (
        children()
      ) : (
        children
      )}
      <ErrorMessage
        error={error?.message}
        noErrorPlaceholder={noErrorPlaceholder}
      />
    </div>
  );

  return (
    <Flex
      direction={orientation === 'horizontal' && label ? 'row' : 'column'}
      align={
        orientation === 'horizontal' && label && noErrorPlaceholder
          ? 'center'
          : 'start'
      }
      gap="2"
      className={clsx(
        className,
        size === 'medium' ? 'w-1/2' : 'w-full',
        size === 'full' ? '' : 'max-w-xl',
      )}
    >
      {label ? (
        <FieldLabel
          id={id}
          label={label}
          labelIcon={labelIcon}
          learnMoreLink={learnMoreLink}
          learnMoreLinkText={learnMoreLinkText}
          tooltipIcon={tooltipIcon}
          description={description}
          tooltip={tooltip}
          loading={loading}
          renderDescriptionLineBreaks={renderDescriptionLineBreaks}
          className={clsx({
            'w-1/2': orientation === 'horizontal',
            'pt-1': orientation === 'horizontal' && !noErrorPlaceholder,
          })}
        />
      ) : null}
      {doNotWrapChildren ? (
        fieldErrors
      ) : (
        <div className={orientation === 'horizontal' ? 'w-1/2' : 'w-full'}>
          {fieldErrors}
        </div>
      )}
    </Flex>
  );
};
