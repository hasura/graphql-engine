import React from 'react';
import { Button } from '../../../../new-components/Button';
import { FaPlayCircle } from 'react-icons/fa';
import { Analytics } from '../../../Analytics';

export interface Props {
  schemaImage: string;
  onRunHandler: () => void;
  onSkipHandler: () => void;
  query: string;
}

export function QueryScreen(props: Props) {
  const { schemaImage, onRunHandler, onSkipHandler, query } = props;
  return (
    <>
      <div className="flex flex-col items-center justify-center overflow-auto bg-gray-200 border border-gray-300 rounded-b p-md mb-md">
        <div>
          <div>
            <img className="mb-4" src={schemaImage} alt="graphql-schema" />
          </div>
          <div className="flex justify-center mb-4">
            <div className="text-sm text-gray-700">
              We&apos;ve created a structure with two tables <b>customer</b> and{' '}
              <b>order</b> connected through a foreign key relationship.
            </div>
          </div>
          <div className="w-full" data-testid="query-dialog-sample-query">
            <pre className="border border-gray-300 bg-gray-100 text-muted font-mono text-sm px-4 py-4">
              <div className="font-semibold text-gray-600 text-xs mb-sm">
                SAMPLE GRAPHQL QUERY
              </div>
              {query}
            </pre>
          </div>
        </div>
      </div>

      <div className="w-full">
        <div className="w-full mb-sm">
          <div className="border border-gray-300 border-l-4 border-l-[#297393] shadow-md rounded bg-white p-md">
            <div className="flex items-center">
              <div className="flex w-3/4 items-center">
                <div className="text-lg text-gray-700 ml-sm">
                  <span className="mr-xs" role="img" aria-label="rocket">
                    🚀
                  </span>
                  <b className="mr-sm">You&apos;re ready to go!</b>
                  Run your first sample query to get started.
                </div>
              </div>
              <div className="flex w-1/4 justify-end">
                <Analytics
                  name="query-screen-get-started-button"
                  passHtmlAttributesToChildren
                >
                  <Button mode="primary" onClick={onRunHandler}>
                    Run a Sample Query <FaPlayCircle />
                  </Button>
                </Analytics>
              </div>
            </div>
          </div>
        </div>
        <div className="flex justify-start items-center w-full">
          <Analytics name="onboarding-skip-button">
            <a
              className="w-auto text-secondary cursor-pointer text-sm hover:text-secondary-dark"
              onClick={() => {
                onSkipHandler();
              }}
            >
              Skip, continue to Console
            </a>
          </Analytics>
        </div>
      </div>
    </>
  );
}
