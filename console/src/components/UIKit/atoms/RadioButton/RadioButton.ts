import styled from 'styled-components';
import {
  color,
  border,
  typography,
  layout,
  space,
  shadow,
} from 'styled-system';
import { RadioButtonProps } from './index';

export const StyledRadioButton = styled.div<RadioButtonProps>`
  [type='radio']:checked,
  [type='radio']:not(:checked) {
    position: absolute;
    left: -9999px;
  }

  [type='radio']:checked + label,
  [type='radio']:not(:checked) + label {
    position: relative;
    padding-left: 28px;
    cursor: pointer;
    line-height: 20px;
    display: inline-block;
    color: #666;
  }

  [type='radio']:checked + label:before,
  [type='radio']:not(:checked) + label:before {
    content: '';
    position: absolute;
    left: 0;
    top: 0;
    width: 20px;
    height: 20px;
    border: 2px solid #484538;
    border-radius: 100%;
    background: #fff;
  }

  [type='radio']:checked + label:before {
    border: 2px solid #1fd6e5;
    transition: all 0.2s ease;
  }

  [type='radio']:hover + label:before {
    border: 2px solid #1fd6e5;
    transition: all 0.2s ease;
  }

  [type='radio']:checked + label:after,
  [type='radio']:not(:checked) + label:after {
    content: '';
    width: 10px;
    height: 10px;
    background: #1fd6e5;
    position: absolute;
    top: 5px;
    left: 5px;
    border-radius: 100%;
    transition: all 0.2s ease;
  }

  [type='radio']:not(:checked) + label:after {
    opacity: 0;
    transform: scale(0);
  }

  [type='radio']:checked + label:after {
    opacity: 1;
    transform: scale(1);
  }

  ${color}
  ${border}
  ${typography}
  ${layout}
  ${space}
  ${shadow}
`;
