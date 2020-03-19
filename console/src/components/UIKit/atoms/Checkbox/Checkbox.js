import styled from 'styled-components';
import {
  color,
  border,
  typography,
  layout,
  space,
  shadow,
} from 'styled-system';

export const StyledCheckBox = styled.div`
  input[type='checkbox'] {
    position: absolute;
    opacity: 0;

    & + label {
      position: relative;
      cursor: pointer;
    }

    & + label:before {
      content: '';
      display: inline-block;
      vertical-align: text-top;
      width: 18px;
      height: 18px;
      background: transparent;
      border: 2px solid #939390;
      border-radius: 2px;
      margin-right: 8px;
    }

    &:hover + label:before {
      border: 2px solid #454236;
    }

    &:checked + label:before {
      background: #f8d721;
      border: 2px solid #f8d721;
    }

    label. &:disabled + label {
      color: #b8b8b8;
      cursor: auto;
    }

    box. &:disabled + label:before {
      box-shadow: none;
      background: #ddd;
    }

    &:checked + label:after {
      content: '';
      position: absolute;
      left: 4px;
      top: 8px;
      background: white;
      width: 2px;
      height: 2px;
      box-shadow: 2px 0 0 white, 4px 0 0 white, 4px -2px 0 white,
        4px -4px 0 white, 4px -6px 0 white, 4px -8px 0 white;
      transform: rotate(45deg);
    }
  }

  ${color}
  ${border}
  ${typography}
  ${layout}
  ${space}
  ${shadow}
`;
