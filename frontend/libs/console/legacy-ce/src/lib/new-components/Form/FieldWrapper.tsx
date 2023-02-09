import * as React from 'react';
import { FieldError } from 'react-hook-form';
import clsx from 'clsx';
import Skeleton from 'react-loading-skeleton';
import { FaExclamationCircle } from 'react-icons/fa';

import type { DiscriminatedTypes } from '@/types';

import { IconTooltip } from '@/new-components/Tooltip';
import { LearnMoreLink } from '../LearnMoreLink';

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
   * Flag indicating wheteher the field is loading
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
} & DiscriminatedTypes<
  {
    /**
     * The field label
     */
    label: string;
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
};

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
    children,
    description,
    tooltip,
    loading,
    noErrorPlaceholder = false,
    renderDescriptionLineBreaks = false,
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
      <label htmlFor={id} className={clsx('block pt-1 text-gray-600 mb-xs')}>
        <span className={clsx('flex items-center')}>
          <span className={clsx('font-semibold', { relative: !!loading })}>
            <FieldLabelIcon />
            {label}
            {loading ? <Skeleton className="absolute inset-0" /> : null}
          </span>
          {!loading && tooltip ? <IconTooltip message={tooltip} /> : null}
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
    </div>
  );
};
