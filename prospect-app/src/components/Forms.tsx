import { useStoreState, useStoreActions } from '../store';
import axios from "../axios";

function Forms() {
    const textIn = useStoreState((state) => state.textIn);
    const textOut = useStoreState((state) => state.textOut);

    const setTextIn = useStoreActions((state) => state.setInText);
    const setTextOut = useStoreActions((state) => state.setOutText);
    const setStatic = useStoreActions((state) => state.setStaticInText);
    const setTimeInvariant = useStoreActions((state) => state.setTimeInvariantInText);
    const setTimeVariant = useStoreActions((state) => state.setTimeVariantInText);

    function generateDataOutput(input: string) {
        axios.get("api/prospect",
            { params: { input: input } }
        ).then((response) => {
            console.log(response.data);
            setTextOut(response.data);
        });
    }

    return (

        <div>
            <textarea value={textIn} placeholder="Put input specifications here" onChange={(e) => setTextIn(e.target.value)}></textarea>
            <br />
            <div className="btn-group">
                <button onClick={() => setStatic()}>Static Example</button>
                <button onClick={() => setTimeInvariant()}>Time Invariant Example</button>
                <button onClick={() => setTimeVariant()}>Time Variant Example</button>
                <button onClick={() => generateDataOutput(textIn)}>Run</button>
            </div>
            <br />
            <textarea readOnly value={textOut} style={{ backgroundColor: "#f6f7cd" }}></textarea>

        </div>

    );
}

export default Forms;