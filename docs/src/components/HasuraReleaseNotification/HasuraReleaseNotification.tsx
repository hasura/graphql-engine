import React, { useState, useEffect } from 'react';
import "./styles.css";

const ENDPOINT_URL = 'https://hasura.io/changelog/api/items?offset=0&product=cloud&limit=10';

const fetchNewReleases = async () => {
  const response = await fetch(ENDPOINT_URL);
  const data = await response.json();
  return data.data.map(entry => entry.version);
};
export const HasuraReleaseNotification = () => {

  useEffect(() => {

    /**
     * Adds or removes the blue dot from the "What's new" link
     * @param {boolean} newReleaseFound
     */
    const updateDot = (newReleaseFound) => {
      const link = document.getElementById('whats-new-link');

      if (newReleaseFound) {
        link.classList.add('blue-dot');
      } else {
        link.classList.remove('blue-dot');
      }
    };

    /**
     * Fetches the latest releases and updates the blue dot
     */
    const fetchReleases = async () => {

      const releases = await fetchNewReleases()

      const seenReleases = JSON.parse(localStorage.getItem('seenReleases')) || [];

      let newReleaseFound = false;

      releases.forEach(release => {
        if (!seenReleases.includes(release)) {
          newReleaseFound = true;
        }
      });

      updateDot(newReleaseFound);
    };

    // Fetch releases on page load
    fetchReleases();

    // Add event listener to "What's new" link to detect when user goes to see new releases on changelog page
    // At the same time update the local storage to mark the new releases as seen
    const whatsNewLink = document.getElementById('whats-new-link');

    whatsNewLink.addEventListener('click', () => {

      fetchNewReleases().then(newReleases => {

        let seenReleases = JSON.parse(localStorage.getItem('seenReleases')) || [];

        newReleases.forEach(release => {
          if (!seenReleases.includes(release)) {
            seenReleases.push(release);
          }
        });

        localStorage.setItem('seenReleases', JSON.stringify(seenReleases));

      })

      // Clear the blue dot
      updateDot(false);

    });
  }, []);

  return null;
};