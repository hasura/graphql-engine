import React from 'react';
import { Button } from '@/new-components/Button';
import { FaCheckCircle, FaGithub, FaTimesCircle } from 'react-icons/fa';
import { MdRefresh } from 'react-icons/md';

const defaultSuccessMessage =
  'Your project from the following GitHub repo has been installed successfully! Get started…';

const defaultErrorMessage =
  'There was a problem loading your project. Please retry loading your sample.';

type GraphiqlPopupProps = {
  status: 'success' | 'error';
  gitRepoName: string;
  gitRepoFullLink: string;
  successMessage?: string;
  errorMessage?: string;
  repoDescription?: string;
  retryCb?: VoidFunction;
  dismissCb: VoidFunction;
};

export function GraphiqlPopup(props: GraphiqlPopupProps) {
  const {
    status,
    gitRepoName,
    gitRepoFullLink,
    successMessage = defaultSuccessMessage,
    errorMessage = defaultErrorMessage,
    repoDescription,
    retryCb,
    dismissCb,
  } = props;

  return (
    <div className="z-20 fixed w-96 bottom-14 right-12 border border-slate-300">
      <div
        className={`p-sm flex space-x-1.5 ${
          status === 'success' ? 'bg-emerald-50' : 'bg-red-50'
        }`}
      >
        {status === 'success' ? (
          <>
            <div>
              <FaCheckCircle className="text-emerald-600" />
            </div>
            <div>{successMessage}</div>
          </>
        ) : (
          <>
            <div>
              <FaTimesCircle className="text-red-600" />
            </div>
            <div>{errorMessage}</div>
          </>
        )}
      </div>
      <div className="p-sm bg-white flex space-x-1.5 border-t border-slate-300 ">
        <div>
          <FaGithub className="text-lg" />
        </div>
        <div>
          <a
            href={gitRepoFullLink}
            target="_blank"
            rel="noopener noreferrer"
            className="text-[#333] hover:text-[#333] hover:no-underline cursor-pointer text-lg mb-sm font-semibold"
          >
            {gitRepoName}
          </a>
          {repoDescription && <div>{repoDescription}</div>}
        </div>
      </div>
      <div className="p-sm bg-slate-50 border-t border-slate-300">
        {status === 'success' ? (
          <Button mode="default" onClick={dismissCb} className="w-full">
            Close and Get Started
          </Button>
        ) : (
          <div className="flex items-center justify-between">
            <Button icon={<MdRefresh />} mode="primary" onClick={retryCb}>
              Retry Loading Sample
            </Button>
            <Button mode="default" onClick={dismissCb}>
              Close
            </Button>
          </div>
        )}
      </div>
    </div>
  );
}
