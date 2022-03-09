// Example from https://reactjs.org/docs/hooks-faq.html#how-to-get-the-previous-props-or-state

import { useEffect, useRef } from 'react';

export function usePrevious(value) {
    const reference = useRef();

    useEffect(() => {
        reference.current = value;
    }, [value]);

    return reference.current;
}