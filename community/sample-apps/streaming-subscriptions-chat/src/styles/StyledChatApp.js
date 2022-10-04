import styled from 'styled-components';

export const ThemeSwitch = styled.div`
  display: flex;
  justify-content: flex-end;
  width: 100%;
  margin-right: 10%;
  margin-bottom: 10px;

  /* The switch - the box around the slider */
  .switch {
    position: relative;
    display: inline-block;
    width: 72px;
    height: 36px;
    padding: 2.5px;
  }

  /* Hide default HTML checkbox */
  .switch input {
    opacity: 0;
    width: 0;
    height: 0;
  }

  /* The slider */
  .slider {
    position: absolute;
    cursor: pointer;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background-color: #1c262f;
    -webkit-transition: 0.4s;
    background: #1c262f;
    /* box-shadow: 3.21429px 1.28571px 8.35714px -1.28571px rgba(0, 0, 0, 0.3),
      0px 2.57143px 3.85714px -0.642857px rgba(0, 0, 0, 0.1); */
    transition: 0.4s;
  }

  .slider:before {
    position: absolute;
    content: '';
    height: 28px;
    width: 28px;
    left: 3px;
    top: 0;
    bottom: 0;
    margin: auto 0;
    -webkit-transition: 0.4s;
    transition: 0.4s;
    box-shadow: 3.21429px 1.28571px 8.35714px -1.28571px rgba(0, 0, 0, 0.3),
      0px 2.57143px 3.85714px -0.642857px rgba(0, 0, 0, 0.1);
    background: #344658
      url('https://graphql-engine-cdn.hasura.io/assets/main-site/bulb_night.svg');
    background-repeat: no-repeat;
    background-position: center;
  }

  input:checked + .slider {
    box-shadow: inset 0px 0px 1.28571px rgba(0, 0, 0, 0.15);
    background: #e7eef3;
  }

  input:checked + .slider:before {
    -webkit-transform: translateX(36px);
    -ms-transform: translateX(36px);
    transform: translateX(36px);
    background: #fff
      url('https://graphql-engine-cdn.hasura.io/assets/main-site/bulb_day.svg');
    background-repeat: no-repeat;
    background-position: center;
  }

  /* Rounded sliders */
  .slider.round {
    border-radius: 34px;
  }

  .slider.round:before {
    border-radius: 50%;
  }
`;

export const StyledBetaAccessForm = styled.div`
  border-radius: 8px;
  padding: 7%;
  margin-left: 24px;

  background: ${({ theme }) => theme.colors.sectionBg};
  box-shadow: ${({ theme }) => theme.boxShadow};

  .header-div {
    min-height: 45px;
    width: 100%;
    display: flex;
    justify-content: space-between;
    align-items: center;

    a {
      display: flex;
      align-items: center;
      color: ${({ theme }) => theme.colors.anchor};
      margin: 0;
      font-size: 16px;
      font-weight: 500;

      svg {
        min-width: 20px;
        margin-left: 8px;
      }
    }
  }

  form {
    margin-top: 31px;
    display: flex;
    flex-direction: column;

    input {
      border: 1px solid;
      outline: none;
      height: 48px;
      border-radius: 4px;
      background: ${({ theme }) => theme.colors.sectionBg};
      border-color: ${({ theme }) => theme.colors.border};
      color: ${({ theme }) => theme.colors.text};
      padding: 8px 10px;
    }

    button {
      width: 100%;
      height: 48px;
      background: #1eb4d4;
      border: none;
      font-weight: 500;
      outline: none;
      margin-top: 16px;
      color: #fff;
      border-radius: 4px;
    }
  }

  .hasura-logo-img {
    min-width: 137px;
    width: 137px;
    max-width: 137px;
  }

  h2 {
    margin-top: 40px;
    font-size: 24px;
    line-height: 1.25;
    font-weight: 700;
    color: ${({ theme }) => theme.colors.heading};
    /* margin-bottom: 8px; */
  }

  p {
    color: ${({ theme }) => theme.colors.text};
    font-size: 16px;
    line-height: 1.75;
    font-weight: 400;
  }

  @media (max-width: 1200px) {
    .hasura-logo-img {
      min-width: 100px;
      width: 100px;
      max-width: 100px;
    }

    .header-div {
      a {
        font-size: 12px;
      }

      svg {
        min-width: 15px;
        width: 15px;
        margin-left: 6px;
      }
    }
  }

  .paperform {
    color: white !important;

    input {
      color: white !important;
    }
  }

  .Paperform__Container {
    .LiveField__answer {
      color: white !important;
    }

    input {
      color: white !important;
    }
  }
`;

export const StyledChatBox = styled.div`
  height: 100vh;
  padding: 24px 0;
  overflow: hidden;

  @media (min-width: 600px) and (max-width: 1010px) {
    width: 70%;
  }

  @media (max-width: 590px) {
    width: 100% !important;
    padding: 0;
  }
`;

export const StyledChatBoxFormDiv = styled.div`
  height: 100%;
  border-radius: 8px;
  /* padding: 32px; */
  box-shadow: ${({ theme }) => theme.boxShadow};
  display: flex;
  overflow-y: auto;
  flex-direction: column;
  justify-content: space-between;

  background: ${({ theme }) => theme.colors.sectionBg};

  .textboxWrapper {
    display: flex;
    border: 1px solid;
    border-color: ${({ theme }) => theme.colors.border};
    border-radius: 4px;
  }

  form {
    background: ${({ theme }) => theme.colors.headerBg};
    min-height: 128px;
    display: flex;
    align-items: center;
    padding: 24px 24px 32px 24px;

    input {
      height: 72px;
      border-top-left-radius: 4px;
      border-bottom-left-radius: 4px;
      background: ${({ theme }) => theme.colors.formInput};
      width: 100%;
      border: none;
      outline: none;
      padding: 8px 12px;
      color: ${({ theme }) => theme.colors.text};
    }

    .form-btn-div {
      min-width: 120px;
      height: 72px;
      display: flex;
      justify-content: center;
      align-items: center;
    }

    button {
      min-width: 88px;
      height: 40px;
      line-height: 40px;
      text-align: center;
      border-radius: 4px;
      outline: none;
      border: none;
      font-size: 16px;
      font-weight: 500;
      background: #344658;
      color: #fff;
      padding: 0;
    }
  }

  .textboxWrapper {
    width: 100%;
  }

  @media (max-width: 590px) {
    form {
      padding: 5px 10px;
    }
  }
`;

export const StyledLeftSection = styled.div`
  width: 25%;
  padding-top: 24px;
  height: 100vh;
  /* overflow-y: auto; */
  margin-right: 24px;
  display: flex;
  flex-direction: column;
  justify-content: space-between;

  @media (min-width: 591px) {
    .hide-on-desk {
      display: none;
    }
  }

  @media (max-width: 590px) {
    width: 100%;
    height: 80px;

    position: fixed;
    z-index: 1;

    .hideOnMobile {
      display: none;
    }

    .mobile-header {
      width: 100%;

      position: absolute;
      top: 0;
      /* background: ${({ theme }) => theme.colors.background}; */
      background: #23303d;

      z-index: 100;

      display: flex;
      align-items: flex-start;

      flex-direction: ${(props) => (props?.showMobileMenu ? 'column' : '')};

      justify-content: space-between;
      padding: 24px 24px 0;

      min-height: ${(props) => (props?.showMobileMenu ? '100vh' : '80px')};
      height: ${(props) => (props?.showMobileMenu ? '100%' : '80px')};
      overflow-y: scroll;

      .mobile-data-wrapper {
        width: 100%;
        margin-top: 35px;

        .close-btn {
          position: absolute;
          top: 24px;
          right: 35px;
          color: #fff;
          font-size: 20px;
        }

        .userList {
          min-height: 30vh;
          max-height: 30vh;
        }
      }

      .hasura-logo-img {
        width: 100px;
      }

      p {
        font-size: 14px;
        color: #fff;
        margin: 0;
      }

      .flex-div {
        display: flex;
        align-items: center;
        margin-top: 8px;

        p {
          margin-top: -2px;
          font-size: 14px;
          font-weight: 600;
        }

        i {
          color: #fff;
          font-size: 18px;
          margin-left: 6px;
          display: block;
        }
      }
    }
  }
`;

export const StyledOnlineUsers = styled.div`
  border-top-left-radius: 12px;
  border-top-right-radius: 12px;
  margin-bottom: 7%;

  .userListHeading {
    font-weight: 500;
    padding: 20px 13px;
    margin-top: 0;
    margin-bottom: 0;

    background: ${({ theme }) => theme.colors.headerBg};
    border-radius: 12px 12px 0px 0px;
    color: ${({ theme }) => theme.colors.heading};
    width: 100%;
    height: 56px;
  }

  .userList {
    padding: 25px 20px;
    display: flex;
    height: 40%;
    min-height: 38.2vh;
    max-height: 38.2vh;
    overflow-y: auto;
    flex: 1;
    flex-wrap: wrap;
    background: ${({ theme }) => theme.colors.sectionBg};
    border-radius: 0px 0px 12px 12px;

    li {
      list-style-type: none;
      min-width: 48px;
      width: 48px;
      min-height: 48px;
      height: 48px;

      border-radius: 50%;
      display: flex;
      justify-content: center;
      align-items: center;
      color: #fff;
      text-transform: uppercase;
      font-weight: 700;
      margin: 12px;
    }

    ::-webkit-scrollbar {
      background: ${({ theme }) =>
        theme.name === 'dark' ? '#1c262f' : '#FAFAFA'};
      width: 12px;
    }

    ::-webkit-scrollbar-thumb {
      background: ${({ theme }) =>
        theme.name === 'dark' ? '#394e60' : '#A6B6C4'};
      border-radius: 80px;
      width: 12px;
    }
  }

  .subscription-stream {
    color: ${({ theme }) => theme.colors.text};
    font-size: 14px;
    font-weight: 500;
  }

  @media (min-width: 1560px) {
    .userList {
      min-height: 40vh;
      max-height: 40vh;
    }
  }

  @media (max-width: 1400px) {
    .userList {
      min-height: 35vh;
      max-height: 35vh;
    }
  }
`;

export const StyledMessage = styled.div`
  width: 92%;
  display: flex;
  justify-content: space-between;
  min-height: 48px;
  height: auto;
  align-items: center;

  font-size: 16px;
  margin: 20px;
  border-radius: 5px;

  .time_stamp {
    font-size: 14px;
    font-weight: 400;
    color: ${({ theme }) => theme.colors.timeStamp};
    font-style: normal;
    /* line-height: 1.7; */
  }

  .messageNameTime {
    display: flex;
    align-items: center;
    flex: 1;
  }

  .messageName {
    min-width: 48px;
    width: 48px;
    min-height: 48px;
    height: 48px;

    border-radius: 50%;
    display: flex;
    justify-content: center;
    align-items: center;
    background: #f669a1;
    background: ${(props) => props?.bgColor};
    color: #fff;
    text-transform: uppercase;
    font-weight: 700;
    margin-right: 16px;
  }

  .messageText {
    color: ${({ theme }) => theme.colors.text};
    font-size: 14px;
    line-height: 1.7;
  }
`;

export const StyledMessagesList = styled.div`
  height: calc(100% - 120px);
  overflow-y: auto;

  ::-webkit-scrollbar {
    background: ${({ theme }) =>
      theme.name === 'dark' ? '#1c262f' : '#FAFAFA'};
    width: 12px;
  }

  ::-webkit-scrollbar-thumb {
    background: ${({ theme }) =>
      theme.name === 'dark' ? '#394e60' : '#A6B6C4'};
    border-radius: 80px;
    width: 12px;
  }

  #newMessage {
    color: ${({ theme }) => theme.colors.text};
  }
`;

export const StyledTypingIndicator = styled.div`
  font-style: italic;
  color: #4f6c86;
  font-size: 12px;
  font-weight: 400;
  line-height: 32px;
  white-space: nowrap;
  position: absolute;
  bottom: 25px;

  @media (min-width: 1450px) {
    bottom: 30px;
  }
`;

export const StyledBanner = styled.div`
  position: -webkit-sticky;
  position: sticky;
  top: 24px;
  background-color: #0c9a70;
  cursor: pointer;
  font-weight: 400;
  padding: 10px 0;
  text-align: center;
  color: #fff;
  min-width: 213px;
  width: 40%;
  height: 36px;
  display: flex;
  justify-content: center;
  align-items: center;
  font-size: 14px;
  margin: 24px auto 0;
  box-shadow: 0px 8px 10px -6px rgba(0, 0, 0, 0.1),
    0px 20px 25px -5px rgba(0, 0, 0, 0.1);
  border-radius: 68px;
`;

export const StyledOnlineUserCircle = styled.li`
  background: ${(props) => props?.bgColor};
`;
