package com.hasura.todo.Todo

import android.app.Dialog
import android.content.Intent
import android.os.Bundle
import android.util.Log
import android.view.View
import android.widget.Button
import androidx.appcompat.app.AppCompatActivity
import com.auth0.android.Auth0
import com.auth0.android.authentication.AuthenticationException
import com.auth0.android.authentication.storage.CredentialsManagerException
import com.auth0.android.authentication.storage.SecureCredentialsManager
import com.auth0.android.callback.BaseCallback
import com.auth0.android.provider.AuthCallback
import com.auth0.android.provider.WebAuthProvider
import com.auth0.android.result.Credentials
import com.hasura.todo.Todo.network.Network


class Login : AppCompatActivity(){

    private var auth0: Auth0? = null
    private val credentialsManager: SecureCredentialsManager? = null

    /**
     * Required when setting up Local Authentication in the Credential Manager
     * Refer to SecureCredentialsManager#requireAuthentication method for more information.
     */
    companion object {
        private const val CODE_DEVICE_AUTHENTICATION = 22
        const val KEY_CLEAR_CREDENTIALS = "com.auth0.CLEAR_CREDENTIALS"
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        //Setup CredentialsManager
        auth0 = Auth0(this)
        auth0?.isLoggingEnabled = true
        auth0?.isOIDCConformant = true

        //Optional - Uncomment the next line to use:
        //Require device authentication before obtaining the credentials
        credentialsManager?.requireAuthentication(this, CODE_DEVICE_AUTHENTICATION, getString(R.string.request_credentials_title), null)

        // Check if the activity was launched after a logout
        if (intent.getBooleanExtra(KEY_CLEAR_CREDENTIALS, false)) {
            credentialsManager?.clearCredentials()
        }

        setContentView(R.layout.activity_login)

        // Check if a log in button must be shown
        if ( credentialsManager == null) {
            val loginButton = findViewById<Button>(R.id.loginButton)
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
            Log.d("Login", exception.toString())
        }

        override fun onSuccess(credentials: Credentials) {
            // Set Apollo Client
            val network = Network()
            network.setApolloClient(credentials.idToken!!, application)
            credentialsManager?.saveCredentials(credentials)
            showNextActivity()
        }
    }

}