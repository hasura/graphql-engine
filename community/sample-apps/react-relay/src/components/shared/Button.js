import styled from "@emotion/styled";

export const Button = styled.button`
  display: inline-block;
  font-weight: 400;
  color: #212529;
  text-align: center;
  vertical-align: middle;
  -webkit-user-select: none;
  -ms-user-select: none;
  user-select: none;
  background-color: transparent;
  border: 1px solid transparent;
  padding: 0.75rem 1.25rem;
  font-size: 1.8rem;
  line-height: 1.5;
  border-radius: 0.25rem;
  transition: color 0.15s ease-in-out, background-color 0.15s ease-in-out,
    border-color 0.15s ease-in-out, box-shadow 0.15s ease-in-out;
  color: #fff;
  background-color: #007bff;
  border-color: #007bff;
  cursor: pointer;
  &:hover {
    color: #fff;
    background-color: #0069d9;
    border-color: #0062cc;
  }
`;
