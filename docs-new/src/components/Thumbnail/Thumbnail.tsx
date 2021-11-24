import React, { useState, useEffect } from 'react';
import ReactDOM from "react-dom";
import BrowserOnly from '@docusaurus/BrowserOnly';
import { CSSTransition } from "react-transition-group";
import styles from './Thumbnail.module.css';

const Modal = (props) => {
  const closeOnEscapeKeyDown = e => {
    if (e.key === "Escape") {
      props.onClose();
    }
  };

  useEffect(() => {
    document.body.addEventListener("keydown", closeOnEscapeKeyDown);
    return () => {
      document.body.removeEventListener("keydown", closeOnEscapeKeyDown);
    };
  }, []);

  return ReactDOM.createPortal(
    <CSSTransition
      in={props.show}
      unmountOnExit
      timeout={{ enter: 0, exit: 300 }}
      classNames={{
        enterDone: styles['enter-done'],
        exit: styles['exit']
      }}
    >
      <div className={styles["modal"]} onClick={props.onClose}>
        <div className={styles["modal-content"]} onClick={e => e.stopPropagation()}>
          <div className={styles["modal-body"]}>{props.children}</div>
          <div className={styles["modal-footer"]}>
            <button onClick={props.onClose} className={styles["button"]}>
              x
            </button>
          </div>
        </div>
      </div>
    </CSSTransition>,
    document.getElementsByTagName("body")[0]
  );
}

const Thumbnail = (props) => {
  const [openModal, setOpenModal] = useState(false);

  return (
    <div className={styles["thumbnail"]}>
      <img {...props} className={styles["main-img"]} onClick={() => setOpenModal(true)} />
      <BrowserOnly>
        {() => (
          <Modal onClose={() => setOpenModal(false)} show={openModal}>
            <img {...props} className={styles["modal-img"]} />
          </Modal>
        )}
      </BrowserOnly>
    </div>
  );
}

export default Thumbnail;