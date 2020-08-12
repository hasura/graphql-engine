const email_input = document.getElementById('mce-EMAIL');
const subscribe_email_input = document.getElementById('floating-mce-EMAIL');
const submit_btn = document.getElementById('mc-embedded-subscribe');
const subscribe_submit_btn = document.getElementById('floating-mc-embedded-subscribe');
// const mcStatusSuccess = document.querySelector('.mce-success-response');
const mcStatusError = document.querySelector('.mce-error-response');
const subscribeMCStatusError = document.querySelector('.floating-mce-error-response');

const subscribeDefaultImg = document.getElementById('subscribe-default');
const subscribeWarningImg = document.getElementById('subscribe-warning');
const subscribeErrorImg = document.getElementById('subscribe-error');
const successMessage = document.getElementById('success-message');

const floatingSubscribeDefaultImg = document.getElementById('floating-subscribe-default');
const floatingSubscribeWarningImg = document.getElementById('floating-subscribe-warning');
const floatingSubscribeErrorImg = document.getElementById('floating-subscribe-error');
const floatingSuccessMessage = document.getElementById('floating-success-message');

email_input.addEventListener('input', function() {
    submit_btn.value = 'Subscribe';
    submit_btn.disabled = false;

    mcStatusError.innerHTML = '';
    mcStatusError.classList.add('hide');
});
subscribe_email_input.addEventListener('input', function() {
    subscribe_submit_btn.value = 'Subscribe';
    subscribe_submit_btn.disabled = false;

    subscribeMCStatusError.innerHTML = '';
    subscribeMCStatusError.classList.add('hide');
});

const readCookie = (name) => {
  const nameEQ = name + "=";
  const ca = document.cookie.split(';');
  for(let i=0;i < ca.length;i++) {
    let c = ca[i];
    while (c.charAt(0)===' ') c = c.substring(1,c.length);
    if (c.indexOf(nameEQ) === 0) return c.substring(nameEQ.length,c.length);
  }
  return null;
}

const showErrorMsg = () => {
    submit_btn.value = 'Subscribe';
    submit_btn.disabled = false;
    subscribe_submit_btn.value = 'Subscribe';
    subscribe_submit_btn.disabled = false;
    mcStatusError.innerHTML = 'Please enter a valid email';
    mcStatusError.classList.remove('hide');
    subscribeMCStatusError.innerHTML = 'Please enter a valid email';
    subscribeMCStatusError.classList.remove('hide');
    clearMsg();
};

const clearMsg = () => {
    setTimeout(function(){
        // mcStatusSuccess.innerHTML = '';
        mcStatusError.innerHTML = '';
        subscribeMCStatusError.innerHTML ='';
        // mcStatusSuccess.classList.add('hide');
        mcStatusError.classList.add('hide');
        subscribeMCStatusError.classList.add('hide');
        submit_btn.disabled = true;
        subscribe_submit_btn.disabled = true;
        //reset input
        email_input.value = '';
        subscribe_email_input.value ='';
        subscribeErrorImg.classList.add('hide');
        subscribeDefaultImg.classList.remove('hide');
        subscribeWarningImg.classList.add('hide');
        floatingSubscribeErrorImg.classList.add('hide');
        floatingSubscribeDefaultImg.classList.remove('hide');
        floatingSubscribeWarningImg.classList.add('hide');

    }, 3000);
}

const submitNewsletterForm = function (form) {
    let gqlEndpoint = 'https://graphql-engine-website.hasura.io/v1/graphql';
    if(window.location.host !== "hasura.io") {
        gqlEndpoint = 'https://graphql-engine-website.hasura-stg.hasura-app.io/v1/graphql';
    }
    // change button state
    if(form.id === 'mc-embedded-subscribe-form') {
      submit_btn.value = 'Subscribing...';
      submit_btn.disabled = true;
    }
    if(form.id === 'floating_mc-embedded-subscribe-form') {
      subscribe_submit_btn.value = 'Subscribing...';
      subscribe_submit_btn.disabled = true;
    }

    const email = form.elements["EMAIL"].value;
    const hbs_context = {
        "hutk": readCookie("hubspotutk"), // include this parameter and set it to the hubspotutk cookie value to enable cookie tracking on your submission
        "pageUri": window.location.host + window.location.pathname,
        "pageName": document.title,
    };
    const gqlMutation = `mutation docsNewsletterSignup($objects: [newsletterSignupInput!]! ) {
      signupNewsletter(objects: $objects) {
        affected_rows
      }
    }`;
    const objects = [{
        "email": email,
        "hbs_context": hbs_context,
        "category": "docs"
    }]
    fetch(gqlEndpoint, {
      method: 'POST',
      body: JSON.stringify({
          query: gqlMutation,
          variables: { objects: objects }
      }),
    })
    .then(response => response.json())
    .then(data => {
        // change button state
        if(form.id === 'mc-embedded-subscribe-form') {
          submit_btn.value = 'Subscribe';
          submit_btn.disabled = false;
        }
        if(form.id === 'floating_mc-embedded-subscribe-form') {
          subscribe_submit_btn.value = 'Subscribe';
          subscribe_submit_btn.disabled = false;
        }
        if(data && data.data && data.data.signupNewsletter.affected_rows) {
            // mcStatusSuccess.innerHTML = 'Thank you for subscribing!';
            // mcStatusSuccess.classList.remove('hide');
            if(form.id === 'mc-embedded-subscribe-form') {
              successMessage.classList.remove('hide');
              submit_btn.classList.add('hide');
              subscribeErrorImg.classList.add('hide');
              subscribeDefaultImg.classList.remove('hide');
              subscribeWarningImg.classList.add('hide');
            }
            if(form.id === 'floating_mc-embedded-subscribe-form') {
              subscribe_submit_btn.classList.add('hide');
              floatingSuccessMessage.classList.remove('hide');
              floatingSubscribeErrorImg.classList.add('hide');
              floatingSubscribeDefaultImg.classList.remove('hide');
              floatingSubscribeWarningImg.classList.add('hide');
            }
        } else {
            if(data.errors && data.errors[0].extensions.code === 'constraint-violation') {
                if(form.id === 'mc-embedded-subscribe-form') {
                  mcStatusError.innerHTML = 'You have already subscribed';
                  subscribeErrorImg.classList.add('hide');
                  subscribeDefaultImg.classList.add('hide');
                  subscribeWarningImg.classList.remove('hide');
                }
                if(form.id === 'floating_mc-embedded-subscribe-form') {
                  subscribeMCStatusError.innerHTML = 'You have already subscribed';
                  floatingSubscribeErrorImg.classList.add('hide');
                  floatingSubscribeDefaultImg.classList.add('hide');
                  floatingSubscribeWarningImg.classList.remove('hide');
                }
            } else {
                if(form.id === 'mc-embedded-subscribe-form') {
                  mcStatusError.innerHTML = 'Something went wrong';
                }
                if(form.id === 'floating_mc-embedded-subscribe-form') {
                  subscribeMCStatusError.innerHTML = 'Something went wrong';
                }
            }
            if(form.id === 'mc-embedded-subscribe-form') {
              mcStatusError.classList.remove('hide');
            }
            if(form.id === 'floating_mc-embedded-subscribe-form') {
              subscribeMCStatusError.classList.remove('hide');
            }
        }
        clearMsg();
    })
    .catch((error) => {
        console.error('Error:', error);
        if(form.id === 'mc-embedded-subscribe-form') {
          submit_btn.value = 'Subscribe';
          submit_btn.disabled = false;
        }
        if(form.id === 'floating_mc-embedded-subscribe-form') {
          subscribe_submit_btn.value = 'Subscribe';
          subscribe_submit_btn.disabled = false;
        }
    });

};

document.addEventListener('submit', function (event) {

    // Only run on forms flagged for newsletter-form validation
    if (!event.target.classList.contains('newsletter-form')) return;

    // Prevent form from submitting
    event.preventDefault();

    // email validation
    const emailPattern = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;

    if(!emailPattern.test(email_input.value)) {
      showErrorMsg();
      subscribeErrorImg.classList.remove('hide');
      subscribeDefaultImg.classList.add('hide');
      subscribeWarningImg.classList.add('hide');
      return;
    }
    // Otherwise, let the form submit normally
    submitNewsletterForm(event.target);


}, false);

document.addEventListener('submit', function (event) {

    // Only run on forms flagged for newsletter-form validation
    if (!event.target.classList.contains('floating-newsletter-form')) return;

    // Prevent form from submitting
    event.preventDefault();

    // email validation
    const emailPattern = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;

    if(!emailPattern.test(subscribe_email_input.value)) {
      showErrorMsg();
      floatingSubscribeErrorImg.classList.remove('hide');
      floatingSubscribeDefaultImg.classList.add('hide');
      floatingSubscribeWarningImg.classList.add('hide');
      return;
    }
    // Otherwise, let the form submit normally
    submitNewsletterForm(event.target);


}, false);
