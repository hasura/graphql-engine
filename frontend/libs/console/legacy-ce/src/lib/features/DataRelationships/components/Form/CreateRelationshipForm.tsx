import React from 'react';
import { CardRadioGroup } from '@/new-components/CardRadioGroup';
import { Driver } from '@/dataSources';
import { DataTarget } from '@/features/Datasources';
import { DbToRsForm } from '@/features/RemoteRelationships';
import { LocalRelationshipWidget } from '../LocalDBRelationshipWidget';
import { RemoteDBRelationshipWidget } from '../RemoteDBRelationshipWidget';
import { formTabs, RelOption } from './utils';

interface CreateFormProps {
  driver: Driver;
  sourceTableInfo: DataTarget;
  /**
   * optional callback function, can be used to get the onComplete event, this could be a onSuccess, or onError event.
   *
   */
  onComplete: (callbackMessage: {
    title?: string;
    message?: string;
    type: 'success' | 'error' | 'cancel';
  }) => void;
}

interface Props extends CreateFormProps {
  option: RelOption;
}

const RenderForm = ({ option, driver, sourceTableInfo, onComplete }: Props) => {
  switch (option) {
    case 'local':
      return (
        <LocalRelationshipWidget
          driver={driver}
          sourceTableInfo={sourceTableInfo}
          onComplete={onComplete}
        />
      );
    case 'remoteDatabase':
      return (
        <RemoteDBRelationshipWidget
          sourceTableInfo={sourceTableInfo}
          onComplete={onComplete}
        />
      );
    case 'remoteSchema':
      return (
        <DbToRsForm sourceTableInfo={sourceTableInfo} onComplete={onComplete} />
      );

    default:
      // This is a TS protection that forces the developer to properly manage all the cases.
      // It throws when the developer adds new values to RelOption without adding a corresponding `case` here.
      throw new Error(`Unknown RelOption: ${option}`);
  }
};

export const CreateRelationshipForm = ({
  sourceTableInfo,
  driver,
  onComplete,
}: CreateFormProps) => {
  const [option, setOption] = React.useState<RelOption>('local');

  return (
    <>
      <p className="mb-sm text-muted font-semibold">
        Select a Relationship Method
      </p>
      <CardRadioGroup items={formTabs} onChange={setOption} value={option} />
      <RenderForm
        option={option}
        driver={driver}
        sourceTableInfo={sourceTableInfo}
        onComplete={onComplete}
      />
    </>
  );
};
