import React from 'react';
import { StoreProvider } from 'easy-peasy';
import { store } from './store';
import InForm from './components/InForm';
import OutForm from './components/OutForm';

function App() {
  return (
    <StoreProvider store={store}>
    <div className="App">
      <h1>PROSPECT Application</h1>
      <InForm />
      <OutForm />
    </div>
    </StoreProvider>
  );
}

export default App;
