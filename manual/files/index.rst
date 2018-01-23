.. .. meta::
   :description: Manual for using File Microservice on Hasura. The microservice lets users to upload and download files with access controls.
   :keywords: hasura, docs, fileStore, file, file upload, file download

Files
=====

File APIs on Hasura lets users upload and store files on a Hasura project and also download when required. The API exposes upload, download and delete methods as well as provide permission options based on user's ID or Role to decide who can create, read or delete files.

.. toctree::
   :maxdepth: 1
  
   Upload<upload>
   Upsert<upsert>
   Download<download>
   Delete<delete>
   Basic permission<permission>
