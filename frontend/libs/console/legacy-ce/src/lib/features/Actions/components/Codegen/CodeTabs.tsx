import React from 'react';
import { useQuery } from '@tanstack/react-query';
import { getFrameworkCodegen } from './utils';
import { getFileExtensionFromFilename } from '@hasura/shared/utils';
import {
  SkeletonList,
  Tabs,
  TabsItem,
  getLanguageModeFromExtension,
  AceEditor,
  Link,
  ErrorCard,
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
  const [selectedTab, setSelectedTab] = React.useState<string | undefined>();

  // Fetching the generated code is a server-synchronisation concern, so it is
  // modelled with react-query instead of a manual effect that synchronously
  // flipped loading/error state (which caused cascading renders). The query key
  // captures every input the result depends on, so the code regenerates
  // whenever the framework, action name, SDL, or derivation settings change.
  const {
    data: codegenFiles = [],
    isFetching: loading,
    error,
    refetch,
  } = useQuery({
    queryKey: [
      'actions-framework-codegen',
      framework,
      currentAction.name,
      actionsSdl,
      shouldDerive,
      shouldDerive ? parentMutation : null,
    ],
    queryFn: () =>
      getFrameworkCodegen(
        framework,
        currentAction.name,
        actionsSdl,
        shouldDerive ? parentMutation : null,
      ),
    enabled: !!framework,
    retry: false,
    refetchOnWindowFocus: false,
  });

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
      <ErrorCard
        showIcon
        error={error}
        headline={
          <>
            Error generating code.&nbsp;
            <Link onClick={() => refetch()} className="cursor-pointer!">
              Try again
            </Link>
          </>
        }
      />
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
