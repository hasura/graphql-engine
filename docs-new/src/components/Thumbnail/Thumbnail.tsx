import React, { useState, useEffect } from 'react';
import ReactDOM from "react-dom";
import { CSSTransition } from "react-transition-group";
import './Thumbnail.css';

const Overlay = (props) => {
  const closeOnEscapeKeyDown = e => {
    if ((e.charCode || e.keyCode) === 27) {
      props.onClose();
    }
  };

  useEffect(() => {
    document.body.addEventListener("keydown", closeOnEscapeKeyDown);
    return function cleanup() {
      document.body.removeEventListener("keydown", closeOnEscapeKeyDown);
    };
  }, []);

  return ReactDOM.createPortal(
    <CSSTransition
      in={props.show}
      unmountOnExit
      timeout={{ enter: 0, exit: 300 }}
    >
      <div className="modal" onClick={props.onClose}>
        <div className="modal-content" onClick={e => e.stopPropagation()}>
          <div className="modal-body">{props.children}</div>
          <div className="modal-footer">
            <button onClick={props.onClose} className="button">
              Close
            </button>
          </div>
        </div>
      </div>
    </CSSTransition>,
    document.getElementsByTagName("body")[0]
  );
}

const Thumbnail = (props) => {
  const [showInModal, setShowInModal] = useState(false);
  return (
  <div
    className={"thumbnail"}
    onClick={() => setShowInModal(true)}
  >
    <img {...props} />
    <div>
      <Overlay onClose={() => setShowInModal(false)} show={showInModal}>
        <img {...props} />
      </Overlay>
    </div>
  </div>
);
}

export default Thumbnail;