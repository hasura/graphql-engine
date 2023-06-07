import * as React from 'react';
import { FieldError } from 'react-hook-form';
import clsx from 'clsx';
import Skeleton from 'react-loading-skeleton';
import { FaExclamationCircle } from 'react-icons/fa';

import type { DiscriminatedTypes } from '../../types';

import { IconTooltip } from '../Tooltip';
import { LearnMoreLink } from '../LearnMoreLink';

export type FieldWrapperPassThroughProps = {
  /**
   * The field ID
   */
  id?: string;
  /**
   * The wrapped field props
   */
  fieldProps?: Partial<
    React.InputHTMLAttributes<
      HTMLInputElement | HTMLSelectElement | HTMLTextAreaElement
    >
  >;
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
  tooltipIcon?: React.ReactElement;
} & DiscriminatedTypes<
  {
    /**
     * The field label
     */
    label?: string;
    /**
     * The field label icon. Can be set only if label is set.
     */
    labelIcon?: React.ReactElement;
    /**
     * The field tooltip label. Can be set only if label is set.
     */
    tooltip?: React.ReactNode;
    /**
     * The link containing more information about the field. Can be set only if label is set.
     */
    learnMoreLink?: string;
  },
  'label'
>;

type FieldWrapperProps = FieldWrapperPassThroughProps & {
  /**
   * The field class
   */
  className?: string;
  /**
   * The field children
   */
  children: React.ReactNode;
  /**
   * The field error
   */
  error?: FieldError | undefined;
  /**
   * Disable wrapping children in layers of divs to enable impacting children with styles (e.g. centering a switch element)
   */
  doNotWrapChildren?: boolean;
};

export const fieldLabelStyles = clsx(
  'block pt-1 text-gray-600 mb-xs font-semibold'
);

export const ErrorComponentTemplate = (props: {
  label: React.ReactNode;
  ariaLabel?: string;
  role?: string;
}) => {
  const ariaAttributes: {
    'aria-label'?: string;
    role?: string;
  } = {};

  if (props.role && props.ariaLabel) {
    ariaAttributes.role = props.role;
    ariaAttributes['aria-label'] = props.ariaLabel;
  }

  return (
    <div
      {...ariaAttributes}
      className="text-red-600 flex items-center text-sm mt-1"
    >
      <span className="flex items-center">{props.label}</span>
    </div>
  );
};

export const FieldWrapper = (props: FieldWrapperProps) => {
  const {
    id,
    labelIcon,
    label,
    learnMoreLink,
    className,
    size = 'full',
    error,
    tooltipIcon,
    children,
    description,
    tooltip,
    loading,
    noErrorPlaceholder = false,
    renderDescriptionLineBreaks = false,
    doNotWrapChildren = false,
  } = props;

  let FieldLabel = () => <></>;
  let FieldLabelIcon = () => <></>;
  let FieldDescription = () => <></>;
  let FieldErrors = () =>
    noErrorPlaceholder ? null : <ErrorComponentTemplate label={<>&nbsp;</>} />;

  if (description) {
    FieldDescription = () => (
      <span
        className={clsx(
          'text-gray-600 mb-xs font-normal text-sm ',
          loading ? 'relative' : ''
        )}
      >
        <span
          className={clsx(renderDescriptionLineBreaks && 'whitespace-pre-line')}
        >
          {description}
        </span>
        {loading ? <Skeleton className="absolute inset-0" /> : null}
      </span>
    );
  }

  if (labelIcon) {
    FieldLabelIcon = () =>
      React.cloneElement(labelIcon, {
        className: 'h-4 w-4 mr-xs',
      });
  }

  if (label) {
    FieldLabel = () => (
      <label htmlFor={id} className={fieldLabelStyles}>
        <span className={clsx('flex items-center')}>
          <span className={clsx('font-semibold', { relative: !!loading })}>
            <FieldLabelIcon />
            {label}
            {loading ? <Skeleton className="absolute inset-0" /> : null}
          </span>
          {!loading && tooltip ? (
            <IconTooltip message={tooltip} icon={tooltipIcon} />
          ) : null}
          {!loading && !!learnMoreLink && (
            <LearnMoreLink href={learnMoreLink} />
          )}
        </span>
        <FieldDescription />
      </label>
    );
  }

  if (error) {
    FieldErrors = () => (
      <ErrorComponentTemplate
        label={
          <>
            <FaExclamationCircle className="fill-current h-4 w-4 mr-xs shrink-0" />
            {props.error?.message}
          </>
        }
        ariaLabel={props.error?.message ?? ''}
        role="alert"
      />
    );
  }

  return (
    <div
      className={clsx(
        className,
        size === 'medium' ? 'w-1/2' : 'w-full',
        size === 'full' ? '' : 'max-w-xl'
      )}
    >
      <FieldLabel />
      {doNotWrapChildren ? (
        <>
          {loading ? (
            <div className={'relative'}>
              {/* Just in case anyone is wondering... we render the children here b/c the height/width of the children takes up the space that the loading skeleton appears on top of. So, without the children, the skeleton would not appear. */}
              {children}
              <Skeleton
                containerClassName="block leading-[0]"
                className="absolute inset-0"
              />
            </div>
          ) : (
            children
          )}
          <FieldErrors />
        </>
      ) : (
        <div>
          {/*
          Remove line height to prevent skeleton bug
        */}
          <div className={loading ? 'relative' : ''}>
            {children}
            {loading && (
              <Skeleton
                containerClassName="block leading-[0]"
                className="absolute inset-0"
              />
            )}
          </div>
          <FieldErrors />
        </div>
      )}
    </div>
  );
};
