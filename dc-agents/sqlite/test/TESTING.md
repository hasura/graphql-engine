# Setting up agent/db

This will create an agent and simple sqlite db for you to be able to interact with in the HGE Console

To finish this setup, you will need a working local Console, so do that first.

1. `cd` into `dc-agents/sqlite` and run

```console
docker build . -t sqlite-agent
```

2. `cd` into `dc-agents/sqlite/test` and run

```console
docker compose up -d
```

3. From the Console, go to **Settings** -> **Feature Flags** and enable **Experimental features for GDC**

4. From the **Data** tab, click **Manage** and click **Add Agent**.
5. Name your agent `sqlite agent` and depending on your docker setup, for URL use `http://localhost:8100` or `http://host.docker.internal:8100`. If you're not sure, try both and see which works!

![Screen Shot 2022-10-07 at 11 06 59 AM](https://user-images.githubusercontent.com/49927862/194598623-5dad962f-a1b0-4db6-9b97-66e71000e344.png)

6. Aftering adding the agent, click `Connect Database` and for the **Data Source Driver** choose `sqlite agent` from the dropdown menu.
7. For **Database Display Name** type in `sqlite-test` and for **db** type in `/chinook.db` and click `Connect Database`

![Screen Shot 2022-10-07 at 11 16 34 AM](https://user-images.githubusercontent.com/49927862/194600350-8131459e-cd91-4ac8-9fcc-3d1b2e491a1f.png)

You should now have this new databse listed on the left: ![Screen Shot 2022-10-07 at 11 12 52 AM](https://user-images.githubusercontent.com/49927862/194599628-952d61e7-1ab8-4c25-8aa2-a9883b9fe6bb.png)
