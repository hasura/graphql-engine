import React from 'react';
import { parse as sdlParse } from 'graphql/language/parser';

import SDLEditor from '../../../../Common/AceEditor/SDLEditor';
import Modal from '../../../../Common/Modal/Modal';
import CloneTypeModal from './CloneTypeModal';
import { getTypesSdl } from '../../../../../shared/utils/sdlUtils';
import {
  Icon,
  ToolTip,
  Heading,
  Text,
  TextLink,
  Flex,
  Box,
} from '../../../../UIKit/atoms';
import styles from './Styles.scss';

const editorLabel = 'New types definition';
const editorTooltip =
  'You can define new GraphQL types that you can use in the action definition above';

const ActionDefinitionEditor = ({
  value,
  onChange,
  className,
  placeholder,
  error,
  timer,
  label = editorLabel,
  tooltip = editorTooltip,
  editorHeight = '200px',
  editorWidth = '600px',
}) => {
  const [modalOpen, setModalState] = React.useState(false);
  const toggleModal = () => setModalState(!modalOpen);

  const onChangeWithError = v => {
    if (timer) {
      clearTimeout(timer);
    }

    const parseDebounceTimer = setTimeout(() => {
      if (v === '') {
        return;
      }
      let _e = null;
      let ast = null;
      try {
        ast = sdlParse(v);
      } catch (e) {
        _e = e;
      }
      onChange(null, _e, null, ast);
    }, 1000);

    onChange(v, null, parseDebounceTimer, null);
  };

  const errorMessage =
    error && (error.message || 'This is not valid GraphQL SDL');

  const handleClonedTypes = types => {
    onChange(`${value}\n\n${getTypesSdl(types)}`);
  };

  return (
    <div className={`${className || ''}`}>
      <Heading type="subHeading" mb="xs">
        {label}
        <ToolTip message={tooltip} ml="sm" />
      </Heading>
      <Box width="600px">
        <Flex mb="5px">
          {error && (
            <Flex>
              <Icon mr="xs" type="close" color="red.primary" />
              <Text color="red.primary">{errorMessage}</Text>
            </Flex>
          )}
          <TextLink ml="auto" mr="20px" onClick={toggleModal} hover="underline">
            <Icon type="copy" mr="xs" size={12} mb="-1px" />
            Clone an existing type
          </TextLink>
          <Modal
            show={modalOpen}
            title={'Clone an existing type'}
            onClose={toggleModal}
            customClass={styles.modal}
          >
            <CloneTypeModal
              handleClonedTypes={handleClonedTypes}
              toggleModal={toggleModal}
            />
          </Modal>
        </Flex>
        <SDLEditor
          name="sdl-editor"
          value={value}
          onChange={onChangeWithError}
          placeholder={placeholder}
          height={editorHeight}
          width={editorWidth}
        />
      </Box>
    </div>
  );
};

export default ActionDefinitionEditor;
