/**
 * Show the appropriate tab content and hide other tab's content
 * @param {string} currentAttrValue The currently selected tab ID.
 * @returns {void}
 */
function showHideTabContent(currentAttrValue) {
    $('.tabs__content').children().
        hide();
    $(`.tabs .tabpanel-${currentAttrValue}`).show();
}

class TabsSingleton {
    constructor(key) {
        this.key = key;
        this.tabStrip = document.querySelector('.tab-strip--singleton');
    }

    get languagePref() {
        return window.localStorage.getItem(this.key);
    }

    set languagePref(value) {
        window.localStorage.setItem(this.key, value);
    }

    /**
     * Return the first singleton tab ID on the page.
     * @returns {string} The first singleton tab ID found.
     */
    getFirstLanguage() {
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
                    this.languagePref = currentAttrValue;
                    this.update();

                    e.preventDefault();
                }
            };
        }

        this.update();
    }

    update() {
        if (!this.tabStrip) { return; }

        let languagePref = this.languagePref;
        if (!languagePref) {
            languagePref = this.getFirstLanguage();
        } else if (!this.tabStrip.querySelector(`[data-tabid="${languagePref}"]`)) {
            // Confirm a tab for their languagePref exists at the top of the page
            languagePref = this.getFirstLanguage();
        }

        if (!languagePref) { return; }

        // Show the appropriate tab content and mark the tab as active
        showHideTabContent(languagePref);
        this.showHideSelectedTab(languagePref);
    }

    /**
     * Marks the selected tab as active, handles special cases for the dropdown
     * @param {string} currentAttrValue The currently selected tab ID.
     * @returns {void}
     */
    showHideSelectedTab(currentAttrValue) {
        // Get the <a>, <li> and <ul> of the selected tab
        const tabLink = $(this.tabStrip.querySelector(`[data-tabid="${currentAttrValue}"]`));
        const tabList = tabLink.parent('ul');

        // Get the dropdown <a> and <li> for active and label management
        const dropdownLink = $(this.tabStrip.querySelector('.dropdown-toggle'));
        const dropdownListItem = $(this.tabStrip.querySelector('.dropdown'));

        // Set the active tab, if it's on the dropdown set it to active and change label
        if (tabList.hasClass('dropdown-menu')) {
            // Use first so text doesn't repeat if more than one set of tabs
            dropdownLink.text(`${tabLink.first().text()}`).append('<span class="caret"></span>');
            dropdownListItem.
                attr('aria-selected', true).
                siblings().
                attr('aria-selected', false);
        } else {
            // Set a non-dropdown tab to active, and change the dropdown label back to "Other"
            tabLink.
                attr('aria-selected', true).
                siblings().
                attr('aria-selected', false);
            dropdownLink.text('Other ').append('<span class="caret"></span>');
        }
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

(new TabsSingleton('languagePref')).setup();
