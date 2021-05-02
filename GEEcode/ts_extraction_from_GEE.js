// Scope: retrieve the MODIS09A1 and landsat profile for a list of sites 
// G. Duveiller - Aug. 2020
// -------------------------------//
  
  // get the data
var valpoints = ee.FeatureCollection("users/gduveiller/ValidationResultsForest"),
MYD13col = ee.ImageCollection("MODIS/006/MYD13Q1"),
L8col = ee.ImageCollection("LANDSAT/LC08/C01/T1_SR"),
roi = 
  /* color: #d63000 */
  /* shown: false */
  /* displayProperties: [
    {
      "type": "rectangle"
    }
  ] */
  ee.Geometry.Polygon(
    [[[10.435749967284531, 70.24253176243823],
      [10.435749967284531, 55.150263969691714],
      [32.23262496728453, 55.150263969691714],
      [32.23262496728453, 70.24253176243823]]], null, false),
L7col = ee.ImageCollection("LANDSAT/LE07/C01/T1_SR");


// subsel
var subsel = valpoints.filter(ee.Filter.eq('Class', '2'));
print('subsel:', subsel);

Map.addLayer(subsel, {"color":"990000"}, 'Projects');
Map.centerObject(subsel, 4);

var size = subsel.size();
print(size);

print(MYD13col);
var ndvi = ee.ImageCollection(MYD13col).select('NDVI').filterDate('2010-01-01', '2019-12-31');  //.filterBounds(place.buffer(200));
print('ndvi:', ndvi);




/**
  * Function to mask clouds based on the pixel_qa band of Landsat 8 SR data.
* @param {ee.Image} image input Landsat 8 SR image
* @return {ee.Image} cloudmasked Landsat 8 image
*/
  function maskL8sr(image) {
    // Bits 3 and 5 are cloud shadow and cloud, respectively.
    var cloudShadowBitMask = (1 << 3);
    var cloudsBitMask = (1 << 5);
    // Get the pixel QA band.
    var qa = image.select('pixel_qa');
    // Both flags should be set to zero, indicating clear conditions.
    var mask = qa.bitwiseAnd(cloudShadowBitMask).eq(0)
    .and(qa.bitwiseAnd(cloudsBitMask).eq(0));
    return image.updateMask(mask);
  }


var l8 = L8col
.filterBounds(roi)
.filterDate('2010-01-01', '2019-12-31')
.map(maskL8sr)
.map(function(image) {
  var ndvi = image.normalizedDifference(['B5', 'B4']).rename('NDVI').float();
  return image.divide(10000).addBands(ndvi);
});
print('Size of Landsat 8 collection', l8.size()); 


var l7 = L7col
.filterBounds(roi)
.filterDate('2010-01-01', '2019-12-31')
.map(maskL8sr)
.map(function(image) {
  var ndvi = image.normalizedDifference(['B4', 'B3']).rename('NDVI').float();
  return image.divide(10000).addBands(ndvi);
});
print('Size of Landsat 7 collection', l7.size()); 



var testPoint = ee.Feature(subsel.first());
print('testPoint:', testPoint);
Map.centerObject(testPoint, 10);


var ts_l8 = l8.map(function(image) {
  return image.select('NDVI').reduceRegions({
    collection: subsel.select(['ID']),
    reducer: ee.Reducer.mean(), 
    scale: 30
  }).filter(ee.Filter.neq('mean', null))
  .map(function(f) { 
    return f.set('imageId', image.id());
  });
}).flatten();

//print(ts_l8.first());
Export.table.toDrive({
  collection: ts_l8, 
  description: 'landsat8NDVIprofiles',
  folder: 'GEE_NDVI4harvestVal',
  fileFormat: 'CSV'
});

var ts_l7 = l7.map(function(image) {
  return image.select('NDVI').reduceRegions({
    collection: subsel.select(['ID']),
    reducer: ee.Reducer.mean(), 
    scale: 30
  }).filter(ee.Filter.neq('mean', null))
  .map(function(f) { 
    return f.set('imageId', image.id());
  });
}).flatten();
print(ts_l7.first());

Export.table.toDrive({
  collection: ts_l7, 
  description: 'landsat7NDVIprofiles',
  folder: 'GEE_NDVI4harvestVal',
  fileFormat: 'CSV'
});



var ts_modis = ndvi.map(function(image) {
  return image.select('NDVI').reduceRegions({
    collection: subsel.select(['ID']),
    reducer: ee.Reducer.mean(),
    scale: 231.656358264,
    crs: 'SR-ORG:6974'
  }).filter(ee.Filter.neq('mean', null))
  .map(function(f) {
    return f.set('imageId', image.id());
  });
}).flatten();

//print(ts_modis.first());
Export.table.toDrive({
  collection: ts_modis, 
  description: 'modisNDVIprofiles',
  folder: 'GEE_NDVI4harvestVal',
  fileFormat: 'CSV'
});
