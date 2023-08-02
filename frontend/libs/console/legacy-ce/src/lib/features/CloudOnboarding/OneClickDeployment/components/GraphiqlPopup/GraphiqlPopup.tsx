import React from 'react';
import { Analytics } from '../../../../Analytics';
import { Button } from '../../../../../new-components/Button';
import { FaCheckCircle, FaGithub, FaTimesCircle } from 'react-icons/fa';
import { MdRefresh } from 'react-icons/md';

const defaultErrorMessage = 'There was a problem setting up your project.';

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
    successMessage,
    errorMessage = defaultErrorMessage,
    // repoDescription,
    retryCb,
    dismissCb,
  } = props;

  let successMsgSection;
  if (successMessage) {
    successMsgSection = <div>{successMessage}</div>;
  } else {
    successMsgSection = (
      <div>
        A new project from{' '}
        <a
          href={gitRepoFullLink}
          target="_blank"
          rel="noopener noreferrer"
          className="text-[#333] hover:text-[#333] hover:no-underline cursor-pointer font-semibold"
        >
          <FaGithub /> {gitRepoName}
        </a>{' '}
        has been set up successfully! Get started by trying your first query
        from the API explorer.
      </div>
    );
  }

  return (
    <div className="z-[103] fixed w-96 bottom-14 right-12 border border-slate-300">
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
            {successMsgSection}
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
      {/*
      <div className="p-sm bg-white flex space-x-1.5 border-t border-slate-300 ">
        <div>
          <FaGithub className="text-lg"/>
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
      */}
      <div className="p-sm bg-slate-50 border-t border-slate-300">
        {status === 'success' ? (
          <Analytics
            name="one-click-deployment-graphiql-popup-get-started"
            passHtmlAttributesToChildren
          >
            <Button mode="default" onClick={dismissCb} className="w-full">
              Close and Get Started
            </Button>
          </Analytics>
        ) : (
          <div className="flex items-center justify-between">
            <Analytics
              name="one-click-deployment-graphiql-popup-retry"
              passHtmlAttributesToChildren
            >
              <Button icon={<MdRefresh />} mode="primary" onClick={retryCb}>
                Retry project set up
              </Button>
            </Analytics>
            <Analytics
              name="one-click-deployment-graphiql-popup-close"
              passHtmlAttributesToChildren
            >
              <Button mode="default" onClick={dismissCb}>
                Close
              </Button>
            </Analytics>
          </div>
        )}
      </div>
    </div>
  );
}
