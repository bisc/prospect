import { useStoreState, useStoreActions } from '../store';
import React, {useState} from 'react';
import { setConstantValue } from 'typescript';
import PROSPECTRun from '../script/PROSPECTRun';

function Forms() {
    const textIn = useStoreState((state) => state.textIn);
    const textOut = useStoreState((state) => state.textOut);
    
    const setTextIn = useStoreActions((state) => state.setInText);
    const setTextOut = useStoreActions((state) => state.setOutText);
    const setStatic = useStoreActions((state) => state.setStaticInText);
    const setTimeInvariant = useStoreActions((state) => state.setTimeInvariantInText);
    const setTimeVariant = useStoreActions((state) => state.setTimeVariantInText);

    return (
        // <div>
        //     <p>Text in: {textIn}</p>
        //     <p>Text out: {textOut}</p>
        //     <button onClick={() => setStatic()}>Static Example</button>
        //     <button onClick={() => setTimeInvariant()}>Time Invariant Example</button>
        // </div>

        <div>
            <textarea value={textIn} placeholder="Put input specifications here" onChange={(e) => setTextIn(e.target.value)}></textarea>
            <div className="btn-group">
                <button onClick={() => setStatic()}>Static Example</button>
                <button onClick={() => setTimeInvariant()}>Time Invariant Example</button>
                <button onClick={() => setTimeVariant()}>Time Variant Example</button>
                <button onClick={() => setTextOut(PROSPECTRun(textIn))}>Run</button>
            </div>

            <textarea readOnly value={textOut} style={{backgroundColor: "#f6f7cd"}}>{textOut}</textarea>

        </div>

    );
}

export default Forms;