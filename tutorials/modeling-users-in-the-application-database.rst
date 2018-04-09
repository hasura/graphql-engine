:orphan:

.. meta::
   :description: Learn how to store user data from the Auth service or user attributes from providers like Google, FB, etc. in your application database
   :keywords: hasura, docs, tutorials, user-modeling, user data
   :content-tags: data-modeling, user-data
   :created-on: 2017-07-25T10:20:35.073Z 

==========================================
Modeling Users in the application database
==========================================


The auth service handles user management related use-cases for your application and store its data in a different, isolated database in the Postgres instance. However, most applications may need to either store custom fields that are not supported by the service or hold a copy of a subset of the user data in the application database i.e. ``hasuradb`` (*or perhaps both*). In this tutorial, we will take a look at how to do this.


Let's get Started!!

Using the Hasura Data Service
=============================

The Hasura Data Service provides you a query interface to the the tables you create on Postgres.

Creating a new Table
--------------------

Go to your project console and head to **Data and Schema Management** present in the left side panel.


Here, create a new table ``user_details``.


Insert the following columns to the ``user_details`` table and click on ``Create``

.. image:: ../img/add-new-table.png

`user_id` will be used to identify which user can access the above fields.

Adding Permissions
------------------

By default, the Hasura Data Service allows only admin to access the table information. To open up access of information to role: user, we add permissions.


Head to **Modify Table, Relationships and Permissions** in the console and scroll to the bottom to find the permissions field


Under Permissions, click on **Add Permissions for new Role** to add a new permission as follows.

.. image:: ../img/add_permissions_userdetails.png

``Check : {"user_id":"X-HASURA-USER-ID"}``

The above statement says that allow access only if value of ``user_id`` equals ``X-HASURA-USER-ID`` where "X-HASURA-USER-ID" is a special variable used by the Hasura Data Service that holds the Hasura-User-Id of the currently logged in user.


**To show how to use the Hasura Data Service, we will be building a simple android app that gets the profile information of the user**


Configuring the Hasura Android SDK
==================================

Once you have created your android project, you will have to add Hasura-Android SDK and a few other dependencies. To do so, I am using Gradle.


Add the following code to the App level build.gradle file

.. code-block:: groovy

	compile 'com.android.support:appcompat-v7:25.3.1'
    	compile 'com.squareup.retrofit2:retrofit:2.1.0'
    	compile 'com.squareup.retrofit2:converter-gson:2.1.0'
    	compile 'com.squareup.okhttp3:logging-interceptor:3.4.1'
    	compile 'com.android.support:recyclerview-v7:25.3.1'
    	compile 'com.github.hasura.android-sdk:sdk:v0.0.4'
    	compile 'com.android.support:design:25.3.1'
    	testCompile 'junit:junit:4.12'

Make the following changes to Project level build.gradle file 

.. code-block::groovy

	allprojects {
        	repositories {
          	jcenter()

          	maven { url 'https://jitpack.io' }
        	}
    	}

For more information go to `Hasura Android SDK <https://github.com/hasura/android-sdk>`_

Initialiazing your Hasura Project
=================================

To access your Hasura Project through android, you will have to first initialize it.


This initialization should be before you start using the SDK(like beginning of your launcher activity), else you will get an error.

.. code-block:: Java

	Hasura.setProjectConfig(new ProjectConfig.Builder()
                .setProjectName("Project-Name")
                .build())
                .enableLogs()
                .initialise(this);

Signup and Login
================

Regarding the signUp/Login part, please refer to `Hasura Android Module 1-Login <https://github.com/hasura/Modules-Android/tree/master/Module_1-Login>`_.


You can implement any one of the methods in the above module for performing SignUp/Login.

Storing the User Details
========================

Check if the User already filled profile details
-------------------------------------------------

We will now make a query to the table we created earlier to check if the user already has filled his profile information, if yes, load that information.


To do this, we have to fire a select query.

**Modelling UserDetails class**

.. code-block:: java

	public class UserDetails {
    		@SerializedName("name")
    		String name;

    		@SerializedName("status")
    		String status;

    		@SerializedName("user_id")
    		int user_id;

    		@SerializedName("file_id")
    		String fileId;

    		public void setName(String name){
    		    this.name = name;
    		}

    		public void setStatus(String status){
    		    this.status = status;
    		}

    		public void setId(int id){
    		    this.user_id = id;
    		}

    		public void setFileId(String fileId){
    		    this.fileId = fileId;
    		}

    		public String getName(){
    		    return name;
    		}

    		public String getStatus(){
    		    return status;
    		}

    		public int getId(){
    		    return user_id;
    		}

    		public String getFileId(){
    		    return fileId;
    		}

    		public UserDetails(){

    		}
		}


**Modelling the SelectQuery**

.. code-block:: Java

	public class SelectQuery {
    		@SerializedName("type")
    		String type = "select";

    		@SerializedName("args")
    		Args args;

    		class Args{
        		@SerializedName("table")
        		String table = "user_details";

        		@SerializedName("columns")
        		String[] columns = {"name","status","user_id","file_id"};

        		@SerializedName("where")
        		Where where;
    		}

    		class Where{
        		@SerializedName("user_id")
        		Integer userId;
    		}

    		public SelectQuery(Integer userId){
        		args = new Args();
        		args.where = new Where();
        		args.where.userId = userId;
    		}
	}

For more about modelling, refer the `Hasura Data Docs <https://hasura.io/_docs/platform/0.6/ref/data/reference.html>`_


To make calls to Hasura, we use the HasuraClient instance provided by the SDK

.. code-block:: Java

	HasuraClient client = Hasura.getClient();

**Making the call**


If we get a successful response, the user has already filled his information earlier.First will use the Hasura FileStore to download the profile picture.


We extract File-Id from the response we get, and then download that particular file.

.. code-block:: Java

	client.useDataService()
                    .setRequestBody(new SelectQuery(userId)
                    .expectResponseTypeArrayOf(UserDetails.class)
                    .enqueue(new Callback<List<UserDetails>, HasuraException>() {
                        @Override
                        public void onSuccess(List<UserDetails> userDetailsList) {
                        	if(userDetailsList.size > 0){}
                            	UserDetails userDetails = userDetailsList.get(0);
                            	String fileId = userDetails.getFileId();
                            
                            	//Now download from file store once you have the fileId of the file.
                        	}
                        }

                        @Override
                        public void onFailure(HasuraException e) {
                        
                        }
                    });


**Using FileStore for downloading data**

.. code-block:: Java

	client.useFileStoreService()
           .downloadFile(fileId, new FileDownloadResponseListener() {
                @Override
                public void onDownloadComplete(byte[] bytes) {
                                            
                }

                @Override
                public void onDownloadFailed(HasuraException e) {
                                            
                }

                @Override
                public void onDownloading(float v) {

                }
           });

**Uploading Data**


Depending on whether the user already has profile information stored or not, we will update/ insert into the table respectively.


**UpdateQuery:**

.. code-block:: Java

	public class UpdateQuery {
    		@SerializedName("type")
    		String type = "update";

    		@SerializedName("args")
    		Args args;

    		class Args{
        		@SerializedName("table")
        		String table = "user_details";

        		@SerializedName("$set")
        		$Set set;

        		@SerializedName("where")
        		Where where;
    		}

    		class $Set{
        		@SerializedName("name")
        		String name;

        		@SerializedName("status")
        		String status;
    		}

    		class Where{
        		@SerializedName("user_id")
        		Integer userId;
    		}

    		public UpdateQuery(UserDetails userDetails){
        		args = new Args();
        		args.set = new $Set();
        		args.set.name = userDetails.getName();
        		args.set.status = userDetails.getStatus();
        		args.where = new Where();
        		args.where.userId = userDetails.getId();
    		}
	}

**InsertQuery:** 

.. code-block:: Java

	public class InsertQuery {
    		@SerializedName("type")
    		String type = "insert";

    		@SerializedName("args")
    		Args args;

    		class Args{
        		@SerializedName("table")
        		String table = "user_details";

        		@SerializedName("objects")
        		List<UserDetails> objects;
    		}

    		public InsertQuery(UserDetails userDetails){
        		args = new Args();
        		args.objects = new ArrayList<>();
        		args.objects.add(userDetails);
    		}
	}

**Making the call:**


When the submit button is clicked, we should add name,status and other details to the userDetails object and upload to the database.

.. code-block:: Java

	button.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                Bitmap bitmap = ((BitmapDrawable)picture.getDrawable()).getBitmap();
                byte[] image = Picture.getBytes(bitmap);
                final UserDetails userDetails = new UserDetails();
                userDetails.setName(name.getText().toString().trim());
                userDetails.setStatus(status.getText().toString().trim());
                userDetails.setId(Hasura.getClient().getUser().getId());

                client.useFileStoreService()
                        .uploadFile(image, "image/*", new FileUploadResponseListener() {
                                 @Override
                                 public void onUploadComplete(FileUploadResponse fileUploadResponse) {
                                     userDetails.setFileId(fileUploadResponse.getFile_id());
                                         client.useDataService()
                                                 .setRequestBody( new "InsertQuery/UpdateQuery"(userDetails))
                                                 .expectResponseType(ResponseMessage.class)
                                                 .enqueue(new Callback<ResponseMessage, HasuraException>() {
                                                     @Override
                                                     public void onSuccess(ResponseMessage responseMessage) {
                                                     
                                                  }

                                                     @Override
                                                     public void onFailure(HasuraException e) {
                                                     
                                                     }
                                                 });
                                     } 
                                 }

                                 @Override
                                 public void onUploadFailed(HasuraException e) {
                                 
                                 }
                        });

            }
        });
	
	
