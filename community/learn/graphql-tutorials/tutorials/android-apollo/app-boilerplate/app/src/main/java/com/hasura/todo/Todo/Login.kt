package com.hasura.todo.Todo

import android.app.Dialog
import android.content.Intent
import android.os.Bundle
import android.view.View
import android.widget.Button
import android.widget.Toast
import androidx.appcompat.app.AppCompatActivity
import com.auth0.android.Auth0
import com.auth0.android.authentication.AuthenticationException
import com.auth0.android.authentication.storage.CredentialsManagerException
import com.auth0.android.authentication.storage.SecureCredentialsManager
import com.auth0.android.callback.BaseCallback
import com.auth0.android.provider.AuthCallback
import com.auth0.android.provider.WebAuthProvider
import com.auth0.android.result.Credentials

class Login : AppCompatActivity(){

    private var auth0: Auth0? = null
    private val credentialsManager: SecureCredentialsManager? = null

    /**
     * Required when setting up Local Authentication in the Credential Manager
     * Refer to SecureCredentialsManager#requireAuthentication method for more information.
     */
    companion object {
        private val CODE_DEVICE_AUTHENTICATION = 22
        val KEY_CLEAR_CREDENTIALS = "com.auth0.CLEAR_CREDENTIALS"
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        //Setup CredentialsManager
        auth0 = Auth0(this)
        auth0?.setLoggingEnabled(true)
        auth0?.setOIDCConformant(true)

        //Optional - Uncomment the next line to use:
        //Require device authentication before obtaining the credentials
        credentialsManager?.requireAuthentication(this, CODE_DEVICE_AUTHENTICATION, getString(R.string.request_credentials_title), null);

        // Check if the activity was launched after a logout
        if (getIntent().getBooleanExtra(KEY_CLEAR_CREDENTIALS, false)) {
            credentialsManager?.clearCredentials()
        }

        setContentView(R.layout.activity_login)

        // Check if a log in button must be shown
        if ( credentialsManager == null) {
            val loginButton = findViewById(R.id.loginButton) as Button
            loginButton.setOnClickListener(object : View.OnClickListener {
                override fun onClick(v: View) {
                    doLogin()
                }
            })
            return
        }

        // Obtain the existing credentials and move to the next activity
        credentialsManager.getCredentials(object : BaseCallback<Credentials, CredentialsManagerException> {
            override fun onSuccess(credentials: Credentials) {
                showNextActivity()
            }

            override fun onFailure(error: CredentialsManagerException) {
                //Authentication cancelled by the user. Exit the app
                finish()
            }
        })

    }

    /**
     * Override required when setting up Local Authentication in the Credential Manager
     * Refer to SecureCredentialsManager#requireAuthentication method for more information.
     */
    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        if (credentialsManager?.checkAuthenticationResult(requestCode, resultCode)!!) {
            return
        }
        super.onActivityResult(requestCode, resultCode, data)
    }

    private fun showNextActivity() {
        val intent = Intent(this@Login, MainActivity::class.java)
        startActivity(intent)
        finish()
    }

    private fun doLogin() {
        WebAuthProvider.init(auth0!!)
            .withScheme("hasura")
            .withAudience(String.format("https://%s/userinfo", getString(R.string.com_auth0_domain)))
            .withScope("openid offline_access")
            .start(this, webCallback)
    }


    private val webCallback = object : AuthCallback {
        override fun onFailure(dialog: Dialog) {
            runOnUiThread { dialog.show() }
        }

        override fun onFailure(exception: AuthenticationException) {
            runOnUiThread { Toast.makeText(this@Login, "Log In - Error Occurred", Toast.LENGTH_SHORT).show() }
        }

        override fun onSuccess(credentials: Credentials) {

            credentialsManager?.saveCredentials(credentials)
            showNextActivity()
        }
    }

}