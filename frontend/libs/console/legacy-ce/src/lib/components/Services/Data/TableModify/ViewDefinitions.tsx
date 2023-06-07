import React from 'react';
import TextAreaWithCopy from '../../../Common/TextAreaWithCopy/TextAreaWithCopy';
import RawSqlButton from '../Common/Components/RawSqlButton';

export interface ViewDefinitionsProps {
  dispatch: () => void;
  sql: string | Record<string, unknown>;
  source: string;
}

const ViewDefinitions: React.FC<ViewDefinitionsProps> = ({
  dispatch,
  sql,
  source,
}) => (
  <div className="w-full sm:w-6/12 mb-md">
    <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
      View Definition
      <span className="ml-sm">
        <RawSqlButton
          sql={sql}
          dispatch={dispatch}
          source={source}
          data-test="modify-view"
        >
          Modify
        </RawSqlButton>
      </span>
    </h4>

    <TextAreaWithCopy
      copyText={sql}
      textLanguage="sql"
      id="copyCustomFunctionSQL"
    />
  </div>
);

export default ViewDefinitions;
