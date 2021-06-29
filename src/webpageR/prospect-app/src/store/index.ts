import { createStore, action, createTypedHooks } from "easy-peasy";
import {InText, OutText, ProspectModel} from './model';
import { staticExample, timeInvariantExample, timeVariantExample } from "./constText";

export const store = createStore<ProspectModel>({
    inText: {
        textIn: "",
        setInText: action((state, payload) => {
            state.textIn = payload;
        }),
        setStaticInText: action((state) => {
            console.log("Doing it");
            state.textIn = staticExample;
        }),
        setTimeInvariantInText: action((state) => {
            state.textIn = timeInvariantExample;
        }),
        setTimeVariantInText: action((state) => {
            state.textIn = timeVariantExample;
        }),
    },
    outText: {
        textOut: "",
        setOutText: action((state, payload) => {
            state.textOut = payload;
        })
    }
});

const typedHooks = createTypedHooks<ProspectModel>();

export const useStoreActions = typedHooks.useStoreActions;
export const useStoreDispatch = typedHooks.useStoreDispatch;
export const useStoreState = typedHooks.useStoreState;

