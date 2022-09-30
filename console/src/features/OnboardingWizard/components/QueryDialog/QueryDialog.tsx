import React from 'react';
import { Dialog } from '@/new-components/Dialog';
import { Button } from '@/new-components/Button';
import { FaPlayCircle } from 'react-icons/fa';

export interface Props {
  title: string;
  description: string;
  query: string;
  schemaImage: string;
  onRunHandler: () => void;
  onSkipHandler: () => void;
}

export function QueryDialog(props: Props) {
  const {
    title,
    description,
    query,
    schemaImage,
    onRunHandler,
    onSkipHandler,
  } = props;
  return (
    <Dialog hasBackdrop title={title} description={description} size="md">
      <>
        <div className="mx-4 my-2">
          <div className="text-md text-gray-700 mb-2">
            We&apos;ve created a <b>`sample`</b> schema to help you get started
            using Hasura. It contains the sample structure of a music directory,
            as well as a view and function to give you an idea about how Hasura
            works.
          </div>
          <img
            className="mb-2"
            data-testid="query-dialog-schema-image"
            src={schemaImage}
            alt="getting-started"
          />
          <div data-testid="query-dialog-sample-query">
            <div className="text-md text-gray-700 mb-2">
              Give it a try with our example query:
            </div>

            <pre className="border border-gray-300 bg-gray-200 text-muted font-mono text-sm px-4 py-4">
              {query}
            </pre>
          </div>
        </div>

        <div className="border border-gray-300 shadow-lg bg-white flex justify-between items-center px-4 py-4">
          <div
            data-trackid="query-dialog-skip-button"
            className="cursor-pointer text-secondary text-sm hover:text-secondary-dark"
            onClick={onSkipHandler}
          >
            Skip getting started tutorial
          </div>
          <Button
            mode="primary"
            data-trackid="query-dialog-get-started-button"
            onClick={onRunHandler}
          >
            Run Query <FaPlayCircle />
          </Button>
        </div>
      </>
    </Dialog>
  );
}
