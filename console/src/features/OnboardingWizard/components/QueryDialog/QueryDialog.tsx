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
    <Dialog hasBackdrop title={title} description={description}>
      <>
        <div className="text-sm px-4 pb-2 font-['Gudea']">
          We’ve created a <b>`sample`</b> schema to help you get started using
          Hasura. It contains the sample structure of a music directory, as well
          as a view and function to give you an idea about how Hasura works.
        </div>
        <img
          className="px-4 pb-1"
          data-testid="query-dialog-schema-image"
          src={schemaImage}
          alt="getting-started"
        />
        <div
          data-testid="query-dialog-sample-query"
          className="bg-[#f8fafc] px-4 pt-1 pb-4"
        >
          <p className="text-muted pb-1 font-light text-sm">
            Give it a try with our example query:
          </p>
          <pre className="bg-gray-200 border-solid border-2 pb-6 px-6 text-muted font-mono text-sm">
            {query}
          </pre>
        </div>
        <div
          box-sizing="border-box"
          className="flex flex-row justify-between border-solid border-2 px-6"
        >
          <div
            data-trackid="query-dialog-skip-button"
            className="pb-2 pt-4 cursor-pointer text-secondary text-sm hover:text-secondary-darkr"
            onClick={onSkipHandler}
          >
            Cancel, continue to my dashboard
          </div>
          <div className="py-2">
            <Button
              mode="primary"
              data-trackid="query-dialog-get-started-button"
              onClick={onRunHandler}
            >
              Run Query <FaPlayCircle />
            </Button>
          </div>
        </div>
      </>
    </Dialog>
  );
}
