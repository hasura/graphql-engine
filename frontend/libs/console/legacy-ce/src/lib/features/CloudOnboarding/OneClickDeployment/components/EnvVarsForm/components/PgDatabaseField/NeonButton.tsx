import React from 'react';
import { IconType } from 'react-icons';
import { MdRefresh } from 'react-icons/md';
import { Analytics } from '@hasura/shared/analytics';
import { FaPlusCircle } from 'react-icons/fa';
import { useFormContext } from 'react-hook-form';
import { Flex } from '@radix-ui/themes';
import { Button, ErrorMessage } from '@hasura/shared/ui';
import { NeonButtonIcons, NeonButtonProps } from '../../types';
import { RequiredEnvVar } from '../../../../types';

export type Props = {
  dbEnvVar: RequiredEnvVar;
  neonButtonProps: NeonButtonProps;
};

const neonButtonIconMap: Record<NeonButtonIcons, IconType> = {
  refresh: MdRefresh,
  create: FaPlusCircle,
};

export function NeonButton(props: Props) {
  const { dbEnvVar, neonButtonProps } = props;
  const { formState } = useFormContext();

  let errorMessage: string | undefined | React.ReactNode;

  if (formState?.errors?.[dbEnvVar.Name]?.message) {
    errorMessage = formState.errors[dbEnvVar.Name]!.message as string;
  }
  if (neonButtonProps.status.status === 'error') {
    errorMessage = neonButtonProps.status.errorDescription;
  }

  return (
    <>
      <Flex align="center">
        <Analytics
          name="one-click-deployment-neon-button"
          passHtmlAttributesToChildren
        >
          <Button
            onClick={neonButtonProps.onClickConnect}
            leftIcon={
              neonButtonProps.icon
                ? neonButtonIconMap[neonButtonProps.icon]
                : undefined
            }
            size="md"
            mode="default"
            loading={neonButtonProps.status.status === 'loading'}
          >
            {neonButtonProps.buttonText}
          </Button>
        </Analytics>
      </Flex>
      <ErrorMessage error={errorMessage} />
    </>
  );
}
