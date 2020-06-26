// Validate the field
var hasError = function (field) {

    // Don't validate submits, buttons, file and reset inputs, and disabled fields
    if (field.disabled || field.type === 'file' || field.type === 'reset' || field.type === 'submit' || field.type === 'button') return;

    // Get validity
    var validity = field.validity;

    // If valid, return null
    if (validity.valid) return;

    // If field is required and empty
    if (validity.valueMissing) return 'Please fill out this field.';

    // If not the right type
    if (validity.typeMismatch) {

        // Email
        if (field.type === 'email') return 'Please enter an email address.';

        // URL
        if (field.type === 'url') return 'Please enter a URL.';

    }

    // If too short
    if (validity.tooShort) return 'Please lengthen this text to ' + field.getAttribute('minLength') + ' characters or more. You are currently using ' + field.value.length + ' characters.';

    // If too long
    if (validity.tooLong) return 'Please shorten this text to no more than ' + field.getAttribute('maxLength') + ' characters. You are currently using ' + field.value.length + ' characters.';

    // If pattern doesn't match
    if (validity.patternMismatch) {

        // If pattern info is included, return custom error
        if (field.hasAttribute('title')) return field.getAttribute('title');

        // Otherwise, generic error
        return 'Please match the requested format.';

    }

    // If number input isn't a number
    if (validity.badInput) return 'Please enter a number.';

    // If a number value doesn't match the step interval
    if (validity.stepMismatch) return 'Please select a valid value.';

    // If a number field is over the max
    if (validity.rangeOverflow) return 'Please select a value that is no more than ' + field.getAttribute('max') + '.';

    // If a number field is below the min
    if (validity.rangeUnderflow) return 'Please select a value that is no less than ' + field.getAttribute('min') + '.';

    // If all else fails, return a generic catchall error
    return 'The value you entered for this field is invalid.';

};

// Show an error message
var showError = function (field, error) {

    // Add error class to field
    field.classList.add('error');

    // If the field is a radio button and part of a group, error all and get the last item in the group
    if (field.type === 'radio' && field.name) {
        var group = field.form.querySelectorAll('[name="' + field.name + '"]');
        if (group.length > 0) {
            for (var i = 0; i < group.length; i++) {
                group[i].classList.add('error');
            }
            field = group[group.length - 1];
        }
    }

    // Get field id or name
    var id = field.id || field.name;
    if (!id) return;

    // Check if error message field already exists
    // If not, create one
    var message = field.form.querySelector('.error-message#error-for-' + id );
    if (!message) {
        message = document.createElement('div');
        message.className = 'error-message';
        message.id = 'error-for-' + id;

        // If the field is a radio button or checkbox, insert error after the label
        var label;
        if (field.type === 'radio' || field.type ==='checkbox') {
            label = field.form.querySelector('label[for="' + id + '"]') || field.parentNode;
            if (label) {
                label.parentNode.insertBefore( message, label.nextSibling );
            }
        }

        // Otherwise, insert it after the field
        if (!label) {
            field.parentNode.insertBefore( message, field.nextSibling );
        }

    }

    // Add ARIA role to the field
    field.setAttribute('aria-describedby', 'error-for-' + id);

    // Update error message
    message.innerHTML = error;

    // Show error message
    message.style.display = 'block';
    message.style.visibility = 'visible';

};


// Remove the error message
var removeError = function (field) {

    // Remove error class to field
    field.classList.remove('error');

    // Remove ARIA role from the field
    field.removeAttribute('aria-describedby');

    // If the field is a radio button and part of a group, remove error from all and get the last item in the group
    if (field.type === 'radio' && field.name) {
        var group = field.form.querySelectorAll('[name="' + field.name + '"]');
        if (group.length > 0) {
            for (var i = 0; i < group.length; i++) {
                group[i].classList.remove('error');
            }
            field = group[group.length - 1];
        }
    }

    // Get field id or name
    var id = field.id || field.name;
    if (!id) return;


    // Check if an error message is in the DOM
    var message = field.form.querySelector('.error-message#error-for-' + id + '');
    if (!message) return;

    // If so, hide it
    message.innerHTML = '';
    message.style.display = 'none';
    message.style.visibility = 'hidden';

};

var serialize = function (form) {

    // Setup our serialized data
    var serialized = '';

    // Loop through each field in the form
    for (i = 0; i < form.elements.length; i++) {

        var field = form.elements[i];

        // Don't serialize fields without a name, submits, buttons, file and reset inputs, and disabled fields
        if (!field.name || field.disabled || field.type === 'file' || field.type === 'reset' || field.type === 'submit' || field.type === 'button') continue;

        // Convert field data to a query string
        if ((field.type !== 'checkbox' && field.type !== 'radio') || field.checked) {
            serialized += '&' + encodeURIComponent(field.name) + "=" + encodeURIComponent(field.value);
        }
    }

    return serialized;

};

// Display the form status
var displayMailChimpStatus = function (data) {

	// change button state
	var submit_btn = document.getElementById('mc-embedded-subscribe');
	submit_btn.value = 'Subscribe';
	submit_btn.disabled = false;

    var mcStatusSuccess = document.querySelector('.mce-success-response');
    var mcStatusError = document.querySelector('.mce-error-response');

    if (!mcStatusSuccess) return;

    // Update our status message
    mcStatusSuccess.innerHTML = data.msg;
    mcStatusSuccess.classList.remove('hide');
    // If error, add error class
    if (data.result === 'error') {
    	mcStatusError.innerHTML = data.msg;
        mcStatusError.classList.remove('hide');
    }
    // setTimeout and reset msgs after 5 seconds
    setTimeout(function(){
    	mcStatusSuccess.innerHTML = '';
    	mcStatusError.innerHTML = '';

    	mcStatusSuccess.classList.add('hide');
    	mcStatusError.classList.add('hide');
      submit_btn.disabled = true;
    	//reset input
		var email_input = document.getElementById('mce-EMAIL');
		email_input.value = '';

    }, 5000);

};

var submitMailChimpForm = function (form) {

	// change button state
	var submit_btn = document.getElementById('mc-embedded-subscribe');
	submit_btn.value = 'Subscribing...';
	submit_btn.disabled = true;

    // Get the Submit URL
    var url = form.getAttribute('action');
    url = url.replace('/post?u=', '/post-json?u=');
    url += serialize(form) + '&c=displayMailChimpStatus';

    // Create script with url and callback (if specified)
    var script = window.document.createElement( 'script' );
    script.src = url;

    // Insert script tag into the DOM (append to <head>)
    var ref = window.document.getElementsByTagName( 'script' )[ 0 ];
    ref.parentNode.insertBefore( script, ref );

    // After the script is loaded (and executed), remove it
    script.onload = function () {
        this.remove();
    };
};

var email_input = document.getElementById('mce-EMAIL');
email_input.addEventListener('input', function() {
	var submit_btn = document.getElementById('mc-embedded-subscribe');
	submit_btn.value = 'Subscribe';
	submit_btn.disabled = false;

	var mcStatusError = document.querySelector('.mce-error-response');
	mcStatusError.innerHTML = '';
    mcStatusError.classList.add('hide');
});

// Listen to all blur events
document.addEventListener('blur', function (event) {

    // Only run if the field is in a form for mailchimp-form
    if (!event.target.form.classList.contains('mailchimp-form')) return;

    // Validate the field
    var error = hasError(event.target);

    // Otherwise, remove any existing error message
    removeError(event.target);

}, true);


document.addEventListener('submit', function (event) {

    // Only run on forms flagged for mailchimp-form validation
    if (!event.target.classList.contains('mailchimp-form')) return;

    // Prevent form from submitting
    event.preventDefault();

    // Get all of the form elements
    var fields = event.target.elements;

    // Validate each field
    // Store the first field with an error to a variable so we can bring it into focus later
    var error, hasErrors;
    for (var i = 0; i < fields.length; i++) {
        error = hasError(fields[i]);
        if (error) {
            showError(fields[i], error);
            if (!hasErrors) {
                hasErrors = fields[i];
            }
        }
    }

    // If there are errrors, don't submit form and focus on first element with error
    if (hasErrors) {
        hasErrors.focus();
        return
    }

    // Otherwise, let the form submit normally
    submitMailChimpForm(event.target);


}, false);
