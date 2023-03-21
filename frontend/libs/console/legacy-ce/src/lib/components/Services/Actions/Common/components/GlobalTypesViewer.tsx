import React, { useRef } from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { getTypesSdl } from '../../../../../shared/utils/sdlUtils';
import AceEditor from '../../../../Common/AceEditor/BaseEditor';
import { customTypesSelector } from '../../../../../metadata/selector';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import { ReduxState } from '../../../../../types';
import GlobalTypesDefIcon from '../../../../Common/Icons/GlobalTypesDef';

const GlobalTypes: React.FC<InjectedProps> = ({ allTypes }) => {
  const existingTypeDefSdl = getTypesSdl(allTypes);
  const editorRef = useRef<any>();
  if (editorRef?.current?.editor?.renderer?.$cursorLayer?.element?.style) {
    editorRef.current.editor.renderer.$cursorLayer.element.style.display =
      'none';
  }

  return (
    <div>
      <label
        htmlFor="types"
        className="flex items-center block text-gray-600 font-medium mb-xs"
      >
        <GlobalTypesDefIcon />
        Declared Global Types
      </label>
      <p className="text-sm text-gray-600 mb-sm">
        Global types which have been declared previously.
      </p>
      <AceEditor
        name="global-types-editor"
        editorRef={editorRef}
        value={existingTypeDefSdl}
        highlightActiveLine={false}
        fontSize="12px"
        height="200px"
        mode="graphqlschema"
        width="100%"
        showPrintMargin={false}
        style={{ background: '#e2e8f0' }}
        setOptions={{
          highlightGutterLine: false,
          useWorker: false,
        }}
        readOnly
      />
    </div>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    allTypes: customTypesSelector(state),
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const GlobalTypesViewer = connector(GlobalTypes);

export default GlobalTypesViewer;
