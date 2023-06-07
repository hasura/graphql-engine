type DataSourceNameInputProps = {
  dataSourceName: string;
  setDataSourceName: (dataSourceName: string) => void;
  dataSources: string[];
  disabled?: boolean;
};

export const DataSourceNameInput = ({
  dataSourceName,
  setDataSourceName,
  dataSources,
  disabled,
}: DataSourceNameInputProps) => {
  return (
    <div className="mb-xs">
      <label
        htmlFor={'datasource-name-input'}
        className={'block pt-1 text-gray-600 mb-xs'}
      >
        <span className={'flex items-center'}>
          <span className={'font-semibold'}>Database</span>
        </span>
      </label>
      <select
        id={'datasource-name-input'}
        data-testid={'datasource-name-input'}
        className={
          'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400 text-gray-500'
        }
        value={dataSourceName}
        onChange={e => setDataSourceName(e.target.value)}
        disabled={disabled}
      >
        {dataSources.map((dataSource, index) => (
          <option key={index} value={dataSource}>
            {dataSource}
          </option>
        ))}
      </select>
    </div>
  );
};
