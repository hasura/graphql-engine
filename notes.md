gateway
redis
auth
data
postgres
shukra
dashboard


Platform update: Gateway
------------------------
- FROM: 1.2.x or 1.x
- Update gateway deployment
- Update shukra deployment
- Update auth deployment

Platform update: Auth
---------------------
- FROM: 1.x
- Update auth deployment

Platform update: Auth (connecting multiple accounts)
----------------------------------------------------
- FROM 2.x
- Upgrade schema job
- Update auth deployment

Update agent
------------
Tries to update the platform every night, or if the user requests for one

Problem
-------
1. Propagate bug-fixes to all services throughout the system
2. Allow the user to update custom services also
3. Versioning code/data
4. Can I rollback to a particular platform version?
