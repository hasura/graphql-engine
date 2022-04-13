const mysql_email_input = document.getElementById('mysql-EMAIL');
const mysql_submit_btn = document.getElementById('mysql-embedded-subscribe');
const mysql_success_status = document.getElementById('mysql-success-response');
const mysql_status_error = document.getElementById('mysql-error-response');

mysql_email_input.addEventListener('input', function() {
    mysql_submit_btn.value = 'Subscribe';
    mysql_submit_btn.disabled = false;

    mysql_status_error.innerHTML = '';
    mysql_status_error.classList.add('hide');
});

const showMySQLErrorMsg = () => {
    mysql_submit_btn.value = 'Subscribe';
    mysql_submit_btn.disabled = false;
    mysql_status_error.innerHTML = 'Please enter a valid email';
    mysql_status_error.classList.remove('hide');
    clearMySQLMsg();
};

const clearMySQLMsg = () => {
    setTimeout(function(){
        mysql_success_status.innerHTML = '';
        mysql_status_error.innerHTML = '';

        mysql_success_status.classList.add('hide');
        mysql_status_error.classList.add('hide');
        mysql_submit_btn.disabled = true;
        //reset input
        mysql_email_input.value = '';
    }, 3000);
}

const submitMySQLForm = function (form) {

    let gqlEndpoint = 'https://graphql-engine-website.hasura.io/v1/graphql';
    if(window.location.host !== "hasura.io") {
        gqlEndpoint = 'https://graphql-engine-website.hasura-stg.hasura-app.io/v1/graphql';
    }
    // change button state
    mysql_submit_btn.value = 'Subscribing...';
    mysql_submit_btn.disabled = true;

    const email = form.elements["EMAIL"].value;

    const hbs_context = {
        "hutk": readCookie("hubspotutk"), // include this parameter and set it to the hubspotutk cookie value to enable cookie tracking on your submission
        "pageUri": window.location.host + window.location.pathname,
        "pageName": document.title,
    };

    const gqlMutation = `mutation notifyDatabaseSupport($object: databaseSupportInput!) {
      notifyDatabaseSupport(object: $object) {
        id
      }
    }`;

    const object = {
      "category": "docs",
      "database": "MySQL",
      "email": email,
      "hbs_context": hbs_context
    };

    fetch(gqlEndpoint, {
      method: 'POST',
      body: JSON.stringify({
          query: gqlMutation,
          variables: { object: object }
      }),
    })
    .then(response => response.json())
    .then(data => {
        // change button state
        mysql_submit_btn.value = 'Subscribe';
        mysql_submit_btn.disabled = false;
        if (data && data.data && data.data.notifyDatabaseSupport.id) {
            mysql_success_status.innerHTML = 'Subscribed!';
            mysql_success_status.classList.remove('hide');
        } else {
            if(data.errors && data.errors[0].message.includes('Uniqueness violation')) {
                mysql_status_error.innerHTML = 'You have already subscribed';
            } else {
                mysql_status_error.innerHTML = 'Something went wrong';
            }
            mysql_status_error.classList.remove('hide');
        }
        clearMySQLMsg();
    })
    .catch((error) => {
        console.error('Error:', error);
        mysql_submit_btn.value = 'Subscribe';
        mysql_submit_btn.disabled = false;
    });

};

document.addEventListener('submit', function (event) {

    // Only run on forms flagged for mysql-subscribe-form validation
    if (!event.target.classList.contains('mysql-subscribe-form')) return;

    // Prevent form from submitting
    event.preventDefault();

    // email validation
    const emailPattern = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;

    if(!emailPattern.test(mysql_email_input.value)) {
      showMySQLErrorMsg();
      return;
    }

    // Otherwise, let the form submit normally
    submitMySQLForm(event.target);


}, false);
