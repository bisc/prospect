import { Action } from 'easy-peasy';

export interface InText {
    textIn: string;
    setInText: Action<InText, string>;
    setStaticInText: Action<InText>;
    setTimeInvariantInText: Action<InText>;
    setTimeVariantInText: Action<InText>;
}

export interface OutText {
    textOut: string;
    setOutText: Action<OutText, string>;
}

export interface ProspectModel {
    inText: InText;
    outText: OutText;
}