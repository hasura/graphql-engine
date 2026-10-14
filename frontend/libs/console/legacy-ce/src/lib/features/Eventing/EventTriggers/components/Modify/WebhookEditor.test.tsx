import { render, screen, fireEvent } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { EventTrigger } from '@hasura/shared/types';
import { URLConf } from '../../types';
import WebhookEditor from './WebhookEditor';

const currentTrigger = {
  name: 'my_trigger',
  webhook: null,
  webhook_from_env: 'MY_ENV',
} as unknown as EventTrigger;

const renderEditor = (webhook: URLConf) => {
  const setWebhook = vi.fn();
  const save = vi.fn();
  const ui = (w: URLConf) => (
    <Theme>
      <WebhookEditor
        currentTrigger={currentTrigger}
        webhook={w}
        setWebhook={setWebhook}
        save={save}
      />
    </Theme>
  );
  const utils = render(ui(webhook));
  return {
    ...utils,
    setWebhook,
    save,
    rerenderWith: (w: URLConf) => utils.rerender(ui(w)),
  };
};

describe('WebhookEditor', () => {
  it('shows the current env webhook value once expanded', () => {
    renderEditor({ type: 'env', value: 'MY_ENV' });

    // expand the editor to reveal the input
    fireEvent.click(screen.getByText('Edit'));

    expect(screen.getByDisplayValue('MY_ENV')).toBeInTheDocument();
  });

  it('syncs the local input when the webhook value prop changes', () => {
    const { rerenderWith } = renderEditor({ type: 'env', value: 'MY_ENV' });

    fireEvent.click(screen.getByText('Edit'));
    expect(screen.getByDisplayValue('MY_ENV')).toBeInTheDocument();

    // a new value from the parent resets the local (editable) value
    rerenderWith({ type: 'env', value: 'OTHER_ENV' });
    expect(screen.getByDisplayValue('OTHER_ENV')).toBeInTheDocument();
  });
});
