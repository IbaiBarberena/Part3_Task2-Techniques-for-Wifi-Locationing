**The objective** is to investigate the possibility of using "WiFi Access Points" to determine somebody’s location inside closed facilities. Aim is to determine which machine learning models work best.

**Data characteristics:**

We have been provided with two different datasets of observations (Wi-Fi fingerprints) for a multi-building university campus. Each observation is associated with a location (building, floor, and location ID). Thus, we are using the signal intensity recorded from multiple "WiFi Access Points" within the building to determine the person's location.

•	**Train dataset**: contains ~19K observations with 520 Wireless Access Points (WAPs), Longitude, Latitude, Floor, Timestamp, BuildingID, Relative Space ID, UserID and Phone ID
•	**Validation dataset**: ~5K observations with 520 WAPs, Longitude, Latitude and Floor are missing. 
Language used: R

