import React from 'react';
import { StoreProvider } from 'easy-peasy';
import { store } from './store';
import Forms from './components/Forms';

function App() {
  return (
    <StoreProvider store={store}>
    <div className="App">
    <div className="Intro">
    <a href="https://precise.seas.upenn.edu/">
    <img src="/precise.logo_bg-white_txt-blue_126x25_transparent.png" />
    </a>
      <h1>PROSPECT Application</h1>
    </div>
      <Forms />
    </div>
    </StoreProvider>
  );
}

export default App;
