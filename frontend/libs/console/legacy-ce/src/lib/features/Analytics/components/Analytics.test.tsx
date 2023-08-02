import React from 'react';
import { render, screen } from '@testing-library/react';

import { Button } from '../../../new-components/Button';
import { REDACT_EVERYTHING } from '../core/heap/getRedactAttributes';
import { Analytics } from './Analytics';

describe('Analytics', () => {
  it('When passed with a name, then should add the data-analytics-name attribute to the children', async () => {
    render(
      <Analytics name="my-name">
        <div>TRACKED DIV</div>
      </Analytics>
    );

    const children = screen.getByText('TRACKED DIV');

    expect(children).toHaveAttribute('data-analytics-name', 'my-name');
    // Protect against unintentionally adding more attributes than expected
    expect(children).toMatchInlineSnapshot(`
      <div
        data-analytics-name="my-name"
      >
        TRACKED DIV
      </div>
    `);
  });

  it('When passed with a name and the legacyTrackIdAttribute option, then should add both the data-analytics-name and data-trackid attributes to the children', async () => {
    render(
      <Analytics name="my-name" legacyTrackIdAttribute>
        <div>TRACKED DIV</div>
      </Analytics>
    );

    const children = screen.getByText('TRACKED DIV');

    expect(children).toHaveAttribute('data-analytics-name', 'my-name');
    expect(children).toHaveAttribute('data-trackid', 'my-name');
    // Protect against unintentionally adding more  than
    expect(children).toMatchInlineSnapshot(`
      <div
        data-analytics-name="my-name"
        data-trackid="my-name"
      >
        TRACKED DIV
      </div>
    `);
  });

  it('When passed with a name and redactText, then should add the correct attributes to the children', async () => {
    render(
      <Analytics name="my-name" redactText>
        <div>REDACTED DIV</div>
      </Analytics>
    );

    const children = screen.getByText('REDACTED DIV');

    expect(children).toHaveAttribute('data-analytics-name', 'my-name');
    expect(children).toHaveAttribute('data-heap-redact-text');
    // Protect against unintentionally adding more  than
    expect(children).toMatchInlineSnapshot(`
      <div
        data-analytics-name="my-name"
        data-heap-redact-text="true"
      >
        REDACTED DIV
      </div>
    `);
  });

  it('When passed with a name some attributes to redact, then should add the correct attributes to the children', async () => {
    render(
      <Analytics name="my-name" redactText htmlAttributesToRedact="id,value">
        <div>REDACTED DIV</div>
      </Analytics>
    );

    const children = screen.getByText('REDACTED DIV');

    expect(children).toHaveAttribute('data-analytics-name', 'my-name');
    expect(children).toHaveAttribute('data-heap-redact-attributes', 'id,value');
    // Protect against unintentionally adding more  than
    expect(children).toMatchInlineSnapshot(`
      <div
        data-analytics-name="my-name"
        data-heap-redact-attributes="id,value"
        data-heap-redact-text="true"
      >
        REDACTED DIV
      </div>
    `);
  });

  it('When passed with the special REDACT_EVERYTHING object, then should include every know attribute that could contain sensitive data', async () => {
    render(
      <Analytics name="my-name" {...REDACT_EVERYTHING}>
        <div>REDACTED DIV</div>
      </Analytics>
    );

    const children = screen.getByText('REDACTED DIV');

    expect(children).toHaveAttribute('data-analytics-name', 'my-name');
    expect(children).toHaveAttribute('data-heap-redact-text');
    expect(children).toHaveAttribute(
      'data-heap-redact-attributes',
      'id,title,data-test,data-element,data-cy,data-testid,data-index-id,data-key,href,value,name,key,placeholder,for'
    );
    // Protect against unintentionally adding more  than
    expect(children).toMatchInlineSnapshot(`
      <div
        data-analytics-name="my-name"
        data-heap-redact-attributes="id,title,data-test,data-element,data-cy,data-testid,data-index-id,data-key,href,value,name,key,placeholder,for"
        data-heap-redact-text="true"
      >
        REDACTED DIV
      </div>
    `);
  });

  it('When the children is a React component, then should wrap it into an display:contents element', async () => {
    const Component = () => <div>REDACTED DIV</div>;

    render(
      <Analytics name="my-name">
        <Component />
      </Analytics>
    );

    const children = screen.getByText('REDACTED DIV');

    // There is no way to access the div added by Analytics
    // eslint-disable-next-line testing-library/no-node-access
    const analyticsWrapper = children.parentElement;

    expect(analyticsWrapper).toHaveStyle('display: contents');
    // Protect against unintentionally adding more  than
    expect(children).toMatchInlineSnapshot(`
      <div>
        REDACTED DIV
      </div>
    `);
  });

  it('When the children is a React component and passHtmlAttributesToChildren is passed, then should pass the additional HTML attributes as props', async () => {
    const Component = (props: Record<string, string>) => (
      <div {...props}>REDACTED DIV</div>
    );

    render(
      <Analytics name="my-name" passHtmlAttributesToChildren>
        <Component />
      </Analytics>
    );

    const children = screen.getByText('REDACTED DIV');

    expect(children).toHaveAttribute('data-analytics-name', 'my-name');
    // Protect against unintentionally adding more  than
    expect(children).toMatchInlineSnapshot(`
      <div
        data-analytics-name="my-name"
      >
        REDACTED DIV
      </div>
    `);
  });

  it('When the children is a string, then should wrap it into an display:contents element', async () => {
    render(<Analytics name="my-name">TRACKED STRING</Analytics>);

    const children = screen.getByText('TRACKED STRING');

    expect(children).toHaveStyle('display: contents');
    // Protect against unintentionally adding more  than
    expect(children).toMatchInlineSnapshot(`
      <div
        data-analytics-name="my-name"
        style="display: contents;"
      >
        TRACKED STRING
      </div>
    `);
  });
});

describe('Analytics misuses', () => {
  beforeEach(() => {
    jest.spyOn(console, 'error').mockImplementation(() => null);
    jest.spyOn(console, 'warn').mockImplementation(() => null);
  });
  afterEach(() => {
    jest.spyOn(console, 'error').mockRestore();
    jest.spyOn(console, 'warn').mockRestore();
  });

  it('When the children is a title tag, then should warn the developer', () => {
    render(
      <Analytics name="my-name">
        <title>TRACKED TITLE</title>
      </Analytics>
    );

    expect(console.error).toHaveBeenCalledWith(
      new Error(
        'title tags cannot be wrapped by <Analytics /> because it will not work, even if used through Helmet (name: "my-name")'
      )
    );
  });

  it('When the children is a DOM element and passHtmlAttributesToChildren is passed, then should warn the developer', () => {
    render(
      <Analytics name="my-name" passHtmlAttributesToChildren>
        <div>TRACKED DIV</div>
      </Analytics>
    );

    expect(console.warn).toHaveBeenCalledWith(
      new Error(
        `Passing HTML attributes is already the default behavior for DOM elements, you can remove the 'passHtmlAttributesToChildren' option passes to the element with name "my-name"`
      )
    );
  });

  it('When the children is a string and passHtmlAttributesToChildren is passed, then should warn the developer', () => {
    render(
      <Analytics name="my-name" passHtmlAttributesToChildren>
        TRACKED STRING
      </Analytics>
    );

    expect(console.error).toHaveBeenCalledWith(
      new Error(
        `Passing HTML attributes to strings is not does not make sense, you must remove the 'passHtmlAttributesToChildren' option passes to the element with name "my-name"`
      )
    );
  });

  it('When the children accepts HTML attributes and passHtmlAttributesToChildren is not passed, then should warn the developer', () => {
    render(
      <Analytics name="my-name">
        <Button>Button</Button>
      </Analytics>
    );

    expect(console.error).toHaveBeenCalledWith(
      new Error(
        `The children accept HTML attributes but you are not using the 'passHtmlAttributesToChildren' prop to the element with name "my-name"`
      )
    );
  });
});
