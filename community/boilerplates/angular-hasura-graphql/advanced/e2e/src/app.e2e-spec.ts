import { browser } from 'protractor';
import { AppPage } from './app.po';

describe('app', () => {
  let page: AppPage;

  beforeEach(() => {
    page = new AppPage();
  });

  it('should display login page and login into app', () => {
    page.navigateTo();
    expect(browser.getCurrentUrl()).toContain('/login');
    page.login();
  });

  it('should display hello message', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('Hello world !');
  });
});
