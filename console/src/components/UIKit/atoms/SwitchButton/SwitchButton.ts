import styled, { css } from 'styled-components';

const checkedStyles = css`
  background-color: #1fd6e5;
  box-shadow: 0 0 1px #1fd6e5;

  :before {
    transform: translateX(20px);
  }
`;

export const StyledSwitchButton = styled.div`
  label {
    position: relative;
    display: inline-block;
    width: 40px;
    height: 20px;

    input {
      opacity: 0;
      width: 0;
      height: 0;
    }

    input:checked {
      background-color: #1fd6e5;
    }

    input:focus {
      box-shadow: 0 0 1px #1fd6e5;
    }

    input:checked {
      transform: translateX(20px);
    }
  }
`;

type StyledSliderProps = {
  checked: boolean;
};

export const StyledSlider = styled.span<StyledSliderProps>`
  position: absolute;
  cursor: pointer;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background-color: #484538;
  transition: 0.4s;
  border-radius: 34px;

  &:before {
    border-radius: 50%;
    position: absolute;
    content: '';
    height: 16px;
    width: 16px;
    left: 2px;
    bottom: 2px;
    background-color: white;
    transition: 0.4s;
  }

  &:hover {
    box-shadow: 0 0 1px #1fd6e5;
  }

  ${({ checked }) => (checked ? checkedStyles : ' ')}
`;
