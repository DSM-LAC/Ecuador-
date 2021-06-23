// Step 1. Create filters

// Center the map on the area of interest.
//var bound = aoi.geometry().bounds();

// Point of interest
// Center the map on the area of interest.

// Point of interest 
var aoi = {
longitude: -80.47,
latitude: -1.22,
};

var radius_of_interest_meters = 50000;
var aoi = ee.Geometry.Point([aoi.longitude, aoi.latitude])
.buffer(radius_of_interest_meters);

var bound = aoi
//var bound = aoi.geometry().bounds();

//Map.addLayer(bound, {}, 'bound');

//Construct start and end dates:
var start = ee.Date('2017-01-01');
var finish = ee.Date('2017-12-31');


// Predictor 5 Landsat 8 Annual NDVI
var ndvi = ee.ImageCollection('LANDSAT/LC08/C01/T1_ANNUAL_NDVI')
            .filterBounds(bound)
            .filterDate(start, finish)
            .reduce('mean')
            .clip(bound);

var ndviVis = {
  min: 0.0,
  max: 19000.0,
  palette: ['bbe029', '0a9501', '074b03'],
};
            
Map.addLayer(ndvi, ndviVis, 'ndvi');      


// Step 3. Export the images, specifying scale and region.
Export.image.toDrive({
  image: ndvi,
  description: 'ndvi',
  maxPixels: 1e11,
  scale: 100,
  region: bound,
  folder: 'bio_enviro'
});
