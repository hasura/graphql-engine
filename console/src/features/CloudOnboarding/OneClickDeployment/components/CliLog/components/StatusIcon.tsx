import React from 'react';
import { FaCheck, FaSpinner, FaTimes } from 'react-icons/fa';
import {
  OneClickDeploymentState,
  ProgressStateStatus,
  UserFacingStep,
} from '../../../types';

export function StatusIcon(props: {
  step: UserFacingStep;
  status: ProgressStateStatus;
}) {
  const { step, status } = props;

  if (status.kind === 'success' && step === OneClickDeploymentState.Completed) {
    return <>🚀</>;
  }

  switch (status.kind) {
    case 'error':
      return <FaTimes className="text-red-500" />;
    case 'success':
      return <FaCheck className="text-emerald-500" />;
    default:
      return <FaSpinner className="animate-spin text-slate-400" />;
  }
}
