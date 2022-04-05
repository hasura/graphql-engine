import React from 'react';

export type RemoteRelOption = 'remoteSchema' | 'remoteDB';

interface RemoteRelRadioCardPickerProps {
  value: RemoteRelOption;
  onChange: (option: RemoteRelOption) => void;
}

export const RemoteRelRadioCardPicker = ({
  value = 'remoteSchema',
  onChange,
}: RemoteRelRadioCardPickerProps) => {
  const isRemoteSchema = value === 'remoteSchema';
  return (
    <div className="grid gap-sm grid-cols-1 sm:grid-cols-2">
      <div
        className={`bg-white shadow-sm rounded p-md border border-gray-300 ${
          isRemoteSchema && 'ring-2 ring-yellow-200 border-yellow-400'
        }`}
      >
        <p className="flex items-center font-semibold text-muted">
          <input
            id="method-remote-schema"
            type="radio"
            value="remoteSchema"
            x-model="relationType"
            name="relationship-method"
            className="cursor-pointer rounded-full border shadow-sm border-gray-300 hover:border-gray-400 focus:ring-yellow-400"
            onChange={e =>
              onChange && onChange(e.target.value as RemoteRelOption)
            }
            checked={isRemoteSchema}
          />
          <label
            htmlFor="method-remote-schema"
            className="cursor-pointer ml-sm"
          >
            Remote Schema
          </label>
        </p>
        <p className="text-muted pl-6">
          Relationship from this remote schema to another remote&nbsp;schema.
        </p>
      </div>
      <div
        className={`bg-white shadow-sm rounded p-md border border-gray-300 ${
          !isRemoteSchema && 'ring-2 ring-yellow-200 border-yellow-400'
        }`}
      >
        <p className="flex items-center font-semibold text-muted">
          <input
            id="method-remote-db"
            type="radio"
            value="remoteDB"
            x-model="relationType"
            name="relationship-method"
            className="cursor-pointer rounded-full border shadow-sm border-gray-300 hover:border-gray-400 focus:ring-yellow-400"
            checked={!isRemoteSchema}
            onChange={e =>
              onChange && onChange(e.target.value as RemoteRelOption)
            }
          />
          <label htmlFor="method-remote-db" className="cursor-pointer ml-sm">
            Remote Database
          </label>
        </p>
        <p className="text-muted pl-6">
          Relationship from this remote schema to a remote database&nbsp;table.
        </p>
      </div>
    </div>
  );
};
