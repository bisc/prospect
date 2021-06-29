import { useStoreState } from '../store';
import React from 'react';

function OutForm() {
    const text = useStoreState((state) => state.outText.textOut);
    return (
        <textarea readOnly style={{backgroundColor: "#f6f7cd"}}>{text}</textarea>
    );
}

export default OutForm;