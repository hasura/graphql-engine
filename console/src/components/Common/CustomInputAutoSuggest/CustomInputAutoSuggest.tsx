import React, { useState } from 'react';
import Autosuggest from 'react-autosuggest';

type Option = {
  title: string
  suggestions: never[]
}

type Suggestion = {
  value: string
}

interface CustomInputAutoSuggestProps extends Autosuggest.InputProps<HTMLInputElement> {
  options: Option[],
  theme?: Autosuggest.Theme,
}

const CustomInputAutoSuggest: React.FC<CustomInputAutoSuggestProps> = props => {
  const [suggestions, setSuggestions] = useState<Option[]>([]);
  // eslint-disable-next-line global-require
  const { options, theme = require('./Theme.scss') } = props;

  const getSuggestions = (value: string): Option[] => {
    const inputValue = value.trim().toLowerCase();
    const inputLength = inputValue.length;
    const filterResults = () => {
      return options.map(option => {
        return {
          title: option.title,
          suggestions: option.suggestions.filter(
            (op: Suggestion) => op.value.toLowerCase().slice(0, inputLength) === inputValue
          ),
        };
      });
    };
    return inputLength === 0 ? [...options] : filterResults();
  };
  const onSuggestionsFetchRequested = (ob: Suggestion) => {
    const { value } = ob;
    setSuggestions(getSuggestions(value));
  };
  const getSuggestionValue = (suggestion: Suggestion) => suggestion.value;
  const onSuggestionsClearRequested = () => {
    setSuggestions([]);
  };
  const renderSuggestion = (suggestion: Suggestion) => <div>{suggestion.value}</div>;

  /* Don't render the section when there are no suggestions in it */
  const renderSectionTitle = (section: Option) => {
    return section.suggestions.length > 0 ? section.title : null;
  };

  const getSectionSuggestions = (section: Option) => {
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

export default CustomInputAutoSuggest;
