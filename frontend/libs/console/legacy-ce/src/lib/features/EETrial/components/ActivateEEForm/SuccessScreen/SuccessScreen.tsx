import React from 'react';
import { Button } from '../../../../../new-components/Button';
import { FaArrowRight, FaCheck } from 'react-icons/fa';
import { Analytics } from '../../../../Analytics';

type Props = {
  /**
   * Show `View Benefits` button in the bottom right
   */
  showBenefitsButton?: boolean;
  /**
   * Callback for the action to be performed on `View Benefits` button click
   */
  onViewBenefitsClick?: VoidFunction;
  /**
   * Callback for the action to be performed on `Close and Continue` button click
   */
  onCloseClick?: VoidFunction;
};

export const SuccessScreen = (props: Props) => {
  const { showBenefitsButton, onViewBenefitsClick, onCloseClick } = props;
  return (
    <>
      <div className="flex flex-col p-md">
        <div className="mb-xs text-gray-50 font-semibold rounded-full bg-gradient-to-br from-yellow-200 to-yellow-600 self-start h-7 w-7 flex items-center justify-center aspect-square mt-1">
          <FaCheck />
        </div>
        <h1 className="text-xl text-slate-900 font-semibold mb-sm">
          Your 30-day trial of Hasura Enterprise has been activated
        </h1>
        <p className="text-muted mt-0 mb-sm">
          <strong>What&apos;s next?</strong>
          <br />
          Please restart your Hasura service in order to start using your new
          Hasura Enterprise features.
        </p>
        <p className="text-muted mt-0 mb-0">
          In Docker, you can restart your container using:
        </p>
        <p className="text-muted font-mono tracking-widest text-sm w-max mb-4">
          docker restart [container-name]
        </p>
        <p className="text-muted mt-0 mb-sm">
          Read our{' '}
          <a
            href="https://hasura.io/docs/latest/enterprise/index"
            target="_blank"
            rel="noopener noreferrer"
            className="text-secondary font-semibold cursor-pointer"
          >
            Hasura Enterprise Edition documentation
          </a>{' '}
          to learn how to get the most out of the features of your trial.
        </p>
      </div>
      <footer className="bg-white border-t border-slate-300 flex justify-between gap-4 px-6 py-3">
        {showBenefitsButton ? (
          <Button onClick={onViewBenefitsClick}>View Benefits</Button>
        ) : null}
        <Analytics name="ee-activation-success-close-and-continue">
          <Button
            mode="primary"
            onClick={onCloseClick}
            icon={<FaArrowRight />}
            iconPosition="end"
          >
            Close and Continue
          </Button>
        </Analytics>
      </footer>
    </>
  );
};
