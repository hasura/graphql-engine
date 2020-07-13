const email_input = document.getElementById('mce-EMAIL');
const submit_btn = document.getElementById('mc-embedded-subscribe');
const mcStatusSuccess = document.querySelector('.mce-success-response');
const mcStatusError = document.querySelector('.mce-error-response');

email_input.addEventListener('input', function() {
    submit_btn.value = 'Subscribe';
    submit_btn.disabled = false;

    const mcStatusError = document.querySelector('.mce-error-response');
    mcStatusError.innerHTML = '';
    mcStatusError.classList.add('hide');
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
    mcStatusError.innerHTML = 'Please enter a valid email';
    mcStatusError.classList.remove('hide');
    clearMsg();
};

const clearMsg = () => {
    setTimeout(function(){
        mcStatusSuccess.innerHTML = '';
        mcStatusError.innerHTML = '';

        mcStatusSuccess.classList.add('hide');
        mcStatusError.classList.add('hide');
        submit_btn.disabled = true;
        //reset input
        email_input.value = '';
    }, 3000);
}

const submitNewsletterForm = function (form) {

    let gqlEndpoint = 'https://graphql-engine-website.hasura.io/v1/graphql';
    if(window.location.host !== "hasura.io") {
        gqlEndpoint = 'https://graphql-engine-website.hasura-stg.hasura-app.io/v1/graphql';
    }
    // change button state
    submit_btn.value = 'Subscribing...';
    submit_btn.disabled = true;

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
        submit_btn.value = 'Subscribe';
        submit_btn.disabled = false;
        if(data && data.data && data.data.signupNewsletter.affected_rows) {
            mcStatusSuccess.innerHTML = 'Thank you for registering!';
            mcStatusSuccess.classList.remove('hide');
        } else {
            if(data.errors && data.errors[0].extensions.code === 'constraint-violation') {
                mcStatusError.innerHTML = 'You have already registered';
            } else {
                mcStatusError.innerHTML = 'Something went wrong';
            }
            mcStatusError.classList.remove('hide');
        }
        clearMsg();
    })
    .catch((error) => {
        console.error('Error:', error);
        submit_btn.value = 'Subscribe';
        submit_btn.disabled = false;
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
      return;
    }

    // Otherwise, let the form submit normally
    submitNewsletterForm(event.target);


}, false);
