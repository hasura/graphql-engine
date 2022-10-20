import React from 'react';
import { render, screen } from '@testing-library/react';
import { Button } from '@/new-components/Button';

describe('Button', () => {
  it('should react on click', () => {
    const onClick = jest.fn();
    render(<Button onClick={onClick}>Hello button</Button>);

    screen
      .getByRole('button', {
        name: /hello button/i,
      })
      .click();

    expect(onClick).toHaveBeenCalled();
  });
});
