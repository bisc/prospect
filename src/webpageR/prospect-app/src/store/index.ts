import { createStore, action, createTypedHooks } from "easy-peasy";
import {ProspectModel} from './model';
import { staticExample, timeInvariantExample, timeVariantExample } from "./constText";
import { setConstantValue } from "typescript";

export const store = createStore<ProspectModel>({
    textIn: "",
    setInText: action((state, payload) => {
        state.textIn = payload;
    }),
    setStaticInText: action((state) => {
        state.textIn = staticExample;
    }),
    setTimeInvariantInText: action((state) => {
        state.textIn = timeInvariantExample;
    }),
    setTimeVariantInText: action((state) => {
        state.textIn = timeVariantExample;
    }),  
    textOut: "",
    setOutText: action((state, payload) => {
        state.textOut = payload;
    })
});

const typedHooks = createTypedHooks<ProspectModel>();

export const useStoreActions = typedHooks.useStoreActions;
export const useStoreDispatch = typedHooks.useStoreDispatch;
export const useStoreState = typedHooks.useStoreState;

