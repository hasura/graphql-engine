import React from 'react';
import { getFrameworkCodegen } from './utils';
import {
  getErrorMessage,
  getFileExtensionFromFilename,
} from '@hasura/shared/utils';
import {
  SkeletonList,
  Tabs,
  TabsItem,
  getLanguageModeFromExtension,
  AceEditor,
  IndicatorCard,
  Link,
} from '@hasura/shared/ui';
import { Action } from '@hasura/shared/types';

type Props = {
  framework: string;
  actionsSdl: string;
  currentAction: Action;
  parentMutation: any;
  shouldDerive: boolean;
};

const CodeTabs = ({
  framework,
  actionsSdl,
  currentAction,
  parentMutation,
  shouldDerive,
}: Props) => {
  const [loading, setLoading] = React.useState(false);
  const [error, setError] = React.useState(null);
  const [codegenFiles, setCodegenFiles] = React.useState([]);
  const [selectedTab, setSelectedTab] = React.useState<string | undefined>();

  const init = () => {
    if (!framework) {
      return;
    }

    setLoading(true);
    setError(null);
    getFrameworkCodegen(
      framework,
      currentAction.name,
      actionsSdl,
      shouldDerive ? parentMutation : null,
    )
      .then((codeFiles) => {
        setCodegenFiles(codeFiles);
        setLoading(false);
      })
      .catch((e) => {
        setError(e);
        setLoading(false);
      });
  };

  React.useEffect(init, [framework, parentMutation, shouldDerive]);

  const files = codegenFiles
    .map(
      ({
        name,
        content,
      }: {
        name: string;
        content: string;
      }): TabsItem | null => {
        const extension = getFileExtensionFromFilename(name);
        if (!extension) {
          return null;
        }

        const mode = getLanguageModeFromExtension(extension);
        const editorProps = {
          mode,
          width: '600px',
          value: content?.trim(),
          readOnly: true,
          setOptions: { useWorker: false },
        };

        return {
          label: name,
          value: name,
          content: <AceEditor {...editorProps} />,
        };
      },
    )
    .filter(Boolean) as TabsItem[];

  // fall back to the first generated file whenever the current selection no
  // longer exists in the file list (e.g. switching framework/template),
  // instead of pointing at a stale tab value.
  const activeTab = files.some((f) => f.value === selectedTab)
    ? selectedTab
    : files[0]?.value;

  if (loading) {
    return <SkeletonList count={12} />;
  }

  if (error) {
    return (
      <IndicatorCard status="negative" showIcon>
        Error generating code.&nbsp;
        <Link onClick={init} className="cursor-pointer!">
          Try again
        </Link>
        <br />
        {getErrorMessage(error)}
      </IndicatorCard>
    );
  }

  return (
    <Tabs
      id="codegen-files-tabs"
      items={files}
      value={activeTab}
      onValueChange={setSelectedTab}
    />
  );
};

export default CodeTabs;
