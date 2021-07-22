import { Action } from 'easy-peasy';


export interface ProspectModel {
    textIn: string;
    setInText: Action<ProspectModel, string>;
    setStaticInText: Action<ProspectModel>;
    setTimeInvariantInText: Action<ProspectModel>;
    setTimeVariantInText: Action<ProspectModel>;
    textOut: string;
    setOutText: Action<ProspectModel, string>;
}