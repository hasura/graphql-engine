Now try entering a new todo in the public list and see what happens in the UI.

If you had done all the previous steps correctly, this is how the notification banner will look like for you now:

![Realtime Feed](../../../assets/realtime-feed.png)

This UI component is written already to be shown when the local react state `showNew` is true. We updated this react state whenever new data came in `client.subscribe`.



