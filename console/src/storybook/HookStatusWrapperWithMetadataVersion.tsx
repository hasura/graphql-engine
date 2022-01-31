import React from 'react';

const HookStatusWrapperWithMetadataVersion: React.FC<{
  status: {
    isLoading: boolean;
    isSuccess: boolean;
    isError: boolean;
    isIdle: boolean;
  };
  metadata: { isSuccess: boolean; version?: number };
}> = ({ status, metadata }) => {
  return (
    <>
      <h2 className="text-lg font-semibold text-muted mb-xs">Status</h2>
      <div className="flex items-center">
        {status.isSuccess ? (
          <span className="flex items-center mr-sm px-sm py-0.5 rounded-full text-sm tracking-wide font-semibold bg-green-100 text-green-800">
            Success
          </span>
        ) : null}
        {status.isError ? (
          <span className="flex items-center mr-sm px-sm py-0.5 rounded-full text-sm tracking-wide font-semibold bg-red-100 text-red-800">
            Failure
          </span>
        ) : null}
        {status.isLoading ? (
          <span className="flex items-center mr-sm px-sm py-0.5 rounded-full text-sm tracking-wide font-semibold bg-gray-100 text-gray-800">
            Loading
          </span>
        ) : null}
        {status.isIdle && !status.isSuccess ? (
          <span className="flex items-center mr-sm px-sm py-0.5 rounded-full text-sm tracking-wide font-semibold bg-purple-100 text-purple-800">
            Idle
          </span>
        ) : null}
      </div>
      <div className="mb-md py-1.5">
        <label className="block mb-xs font-semibold text-muted">
          Metadata version
        </label>
        <input
          disabled
          type="text"
          className="block w-full h-input cursor-not-allowed rounded border bg-gray-100 border-gray-100"
          placeholder="This is a sample input..."
          value={metadata.isSuccess ? metadata.version : undefined}
        />
      </div>
    </>
  );
};

export default HookStatusWrapperWithMetadataVersion;
