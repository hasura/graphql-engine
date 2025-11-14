import React, { useEffect } from 'react';
import './styles.css';

const App = () => {
  const toggleDarkMode = () => {
    const isDark = document.body.classList.toggle('dark-mode');
    localStorage.setItem('theme', isDark ? 'dark' : 'light');
  };

  useEffect(() => {
    const savedTheme = localStorage.getItem('theme');
    if (savedTheme === 'dark') {
      document.body.classList.add('dark-mode');
    }
  }, []);

  return (
    <div>
      {/* Existing Hasura Console UI goes here */}

      <button
        onClick={toggleDarkMode}
        style={{
          margin: '1rem',
          padding: '10px 20px',
          borderRadius: '5px',
          border: 'none',
          backgroundColor: '#333',
          color: 'white',
          cursor: 'pointer',
        }}
      >
        🌙 Toggle Dark Mode
      </button>
    </div>
  );
};

export default App;
