/**
 * Show the appropriate tab content and hide other tab's content
 * @param {string} currentAttrValue The currently selected tab ID.
 * @returns {void}
 */
function showHideTabContent(currentAttrValue) {
    $('.tabs__content').children().
        hide();
    $(`.global-tabs .tabpanel-${currentAttrValue}`).show();
}

class TabsSingleton {
    constructor(key) {
        this.key = key;
        this.tabStrip = document.querySelector('.tab-strip--singleton');
    }

    get tabPref() {
        return window.localStorage.getItem(this.key);
    }

    set tabPref(value) {
        window.localStorage.setItem(this.key, value);
    }

    /**
     * Return the first singleton tab ID on the page.
     * @returns {string} The first singleton tab ID found.
     */
    getFirstTab() {
        const tabsElement = this.tabStrip.querySelector('.tab-strip__element[aria-selected=true]');
        if (!tabsElement) { return null; }

        return tabsElement.getAttribute('data-tabid');
    }

    setup() {
        if (!this.tabStrip) { return; }

        this.hideTabBars();

        for (const element of this.tabStrip.querySelectorAll('[data-tabid]')) {
            element.onclick = (e) => {
                // Get the tab ID of the clicked tab
                const currentAttrValue = e.target.getAttribute('data-tabid');

                // Check to make sure value is not null, i.e., don't do anything on "other"
                if (currentAttrValue) {
                    // Save the users preference and re-render
                    this.tabPref = currentAttrValue;
                    this.update();

                    e.preventDefault();
                }
            };
        }

        this.update();
    }

    update() {
        if (!this.tabStrip) { return; }

        let tabPref = this.tabPref;
        if (!tabPref) {
            tabPref = this.getFirstTab();
        } else if (!this.tabStrip.querySelector(`[data-tabid="${tabPref}"]`)) {
            // Confirm a tab for their tabPref exists at the top of the page
            tabPref = this.getFirstTab();
        }

        if (!tabPref) { return; }

        // Show the appropriate tab content and mark the tab as active
        showHideTabContent(tabPref);
        this.showHideSelectedTab(tabPref);
    }

    /**
     * Marks the selected tab as active
     * @param {string} currentAttrValue The currently selected tab ID.
     * @returns {void}
     */
    showHideSelectedTab(currentAttrValue) {
        // Get the <a>, <li> and <ul> of the selected tab
        const tabLink = $(this.tabStrip.querySelector(`[data-tabid="${currentAttrValue}"]`));
        // Set a tab to active
        tabLink.
            attr('aria-selected', true).
            siblings().
            attr('aria-selected', false);
    }

    /**
     * Show only the first set of tabs at the top of the page.
     * @returns {void}
     */
    hideTabBars() {
        const tabBars = $('.tab-strip--singleton');
        const mainTabBar = tabBars.first();
        // Remove any additional tab bars
        tabBars.slice(1).
            detach();
        // Position the main tab bar after the page title
        mainTabBar.
            detach().
            insertAfter('h1').
            first();
    }
}

(new TabsSingleton('tabPref')).setup();
