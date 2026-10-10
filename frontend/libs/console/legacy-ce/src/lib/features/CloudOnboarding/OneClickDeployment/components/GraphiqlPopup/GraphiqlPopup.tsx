import React from 'react';
import { Analytics } from '@hasura/shared/analytics';
import { Button, IndicatorCard, Separator } from '@hasura/shared/ui';
import { FaGithub } from 'react-icons/fa';
import { MdRefresh } from 'react-icons/md';
import { Flex, Link } from '@radix-ui/themes';

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
        <Link href={gitRepoFullLink} target="_blank" rel="noopener noreferrer">
          <FaGithub className="inline-block" /> {gitRepoName}
        </Link>{' '}
        has been set up successfully! Get started by trying your first query
        from the API explorer.
      </div>
    );
  }

  return (
    <div className="z-103 fixed w-96 bottom-14 right-12 border border-slate-300">
      <IndicatorCard
        status={status === 'success' ? 'positive' : 'negative'}
        className={`p-sm flex space-x-1.5`}
        showIcon
      >
        {status === 'success' ? successMsgSection : errorMessage}
      </IndicatorCard>
      {/*
      <div className="p-2 bg-white flex space-x-1.5 border-t border-slate-300 ">
        <div>
          <FaGithub className="text-lg"/>
        </div>
        <div>
          <a
            href={gitRepoFullLink}
            target="_blank"
            rel="noopener noreferrer"
            className="text-[#333] hover:text-[#333] hover:no-underline cursor-pointer text-lg mb-2 font-semibold"
          >
            {gitRepoName}
          </a>
          {repoDescription && <div>{repoDescription}</div>}
        </div>
      </div>
      */}
      <Separator size="4" />
      <div className="p-2">
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
          <Flex align="center" justify="between">
            <Analytics
              name="one-click-deployment-graphiql-popup-retry"
              passHtmlAttributesToChildren
            >
              <Button leftIcon={MdRefresh} mode="primary" onClick={retryCb}>
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
          </Flex>
        )}
      </div>
    </div>
  );
}
