import { StoreProvider } from 'easy-peasy';
import { store } from './store';
import Forms from './components/Forms';

function App() {

  return (
    <StoreProvider store={store}>
      <div className="App">
        <div className="Intro">
          <h1><a href="/">PROSPECT Application</a></h1>
        </div>
        <Forms />
      </div>
    </StoreProvider>
  );
}

export default App;
