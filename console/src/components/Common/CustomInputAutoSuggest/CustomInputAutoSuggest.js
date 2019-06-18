import React, { useState } from 'react';
import PropTypes from 'prop-types';
import Autosuggest from 'react-autosuggest';

const CustomInputAutoSuggest = props => {
  const [suggestions, setSuggestions] = useState([]);

  const { options, theme = require('./Theme.scss') } = props;

  const getSuggestions = value => {
    const inputValue = value.trim().toLowerCase();
    const inputLength = inputValue.length;
    const filterResults = () => {
      return options.map(option => {
        return {
          title: option.title,
          suggestions: option.suggestions.filter(
            op => op.value.toLowerCase().slice(0, inputLength) === inputValue
          ),
        };
      });
    };
    return inputLength === 0 ? [...options] : filterResults();
  };
  const onSuggestionsFetchRequested = ob => {
    const { value } = ob;
    setSuggestions(getSuggestions(value));
  };
  const getSuggestionValue = suggestion => suggestion.value;
  const onSuggestionsClearRequested = () => {
    setSuggestions([]);
  };
  const renderSuggestion = suggestion => <div>{suggestion.value}</div>;

  /* Don't render the section when there are no suggestions in it */
  const renderSectionTitle = section => {
    return section.suggestions.length > 0 ? section.title : null;
  };

  const getSectionSuggestions = section => {
    return section.suggestions;
  };

  return (
    <Autosuggest
      suggestions={suggestions}
      onSuggestionsFetchRequested={onSuggestionsFetchRequested}
      onSuggestionsClearRequested={onSuggestionsClearRequested}
      getSuggestionValue={getSuggestionValue}
      renderSuggestion={renderSuggestion}
      inputProps={{ ...props }}
      theme={theme}
      multiSection
      renderSectionTitle={renderSectionTitle}
      shouldRenderSuggestions={() => true}
      getSectionSuggestions={getSectionSuggestions}
    />
  );
};

CustomInputAutoSuggest.propTypes = {
  options: PropTypes.array.isRequired,
};

export default CustomInputAutoSuggest;
