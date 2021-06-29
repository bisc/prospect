import { useStoreState, useStoreActions } from '../store';
import React, {useState} from 'react';

function InForm() {
    const text = useStoreState((state) => state.inText.textIn);
    
    const setText = useStoreActions((state) => state.inText.setInText);
    const setStatic = useStoreActions((state) => state.inText.setStaticInText);
    const setTimeInvariant = useStoreActions((state) => state.inText.setTimeInvariantInText);
    const setTimeVariant = useStoreActions((state) => state.inText.setTimeVariantInText);

    return (
        <div>
            <textarea placeholder="Put input specifications here" onChange={() => setText}>{text}</textarea>
            <div className="btn-group">
                <button onClick={() => setStatic()}>Static Example</button>
                <button>Time Invariant Example</button>
                <button>Time Variant Example</button>
                <button>Run</button>
            </div>
        </div>

    );
}

export default InForm;