import React from 'react';
import { render, screen } from '@testing-library/react';
import { Button } from '.';

describe('Button', () => {
  it('When clicked, then should call the onClick callback', () => {
    const onClick = jest.fn();
    render(<Button onClick={onClick}>Hello button</Button>);

    screen
      .getByRole('button', {
        name: /hello button/i,
      })
      .click();

    expect(onClick).toHaveBeenCalled();
  });

  it('When passed with custom attributes, then should add the same attributes the button', () => {
    render(<Button data-foo="bar">Hello button</Button>);

    const button = screen.getByRole('button', {
      name: /hello button/i,
    });

    expect(button).toHaveAttribute('data-foo', 'bar');
  });
});
