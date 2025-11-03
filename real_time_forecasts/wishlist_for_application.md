# Wishlist for application (in another world where I had time)

user behaviors to support? e.g. regional selection, point selection, search by latitude, longitude, downloading data, displaying values, placing markers? 

- BUG FIX: Selecting and creating markers was broken for auxiliary leaflet plots. Must investigate code. 
To reestablish behavior, edit values of `leafletClicks` and `leafletMarkerClicks` in `app.R`.

- enable region highlighting
- support rearranging of highlight plot (crab example)
- optimize data retrieval for monthly averages….
- consider line plot of value over past year?????
- fix caching issues
- replace updateMarkers with more optimized function (? prevent rerenders)
- responsive design
- create information tab with plot of ecomon data
- create warnings for high uncertainty points & regions outside of training data