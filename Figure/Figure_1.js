
var table: Table projects/ee-renaudkoukoui/assets/watershed
var collection= ee.ImageCollection("projects/sat-io/open-datasets/landcover/ESRI_Global-LULC_10m_TS")
                  .filterDate('2017-01-01', '2017-12-31');


print(collection);

//Definition du dictionnaire
var dict= {
 "names": [
   "Water",
   "Trees",
   "Grass",
   "Flooded Vegetation",
   "Crops",
   "Scrub/Shrub",
   "Built Area",
   "Bare Ground",
   "Snow/ Ice",
   "Clouds"
   ],
   "colors": [
     "#1A5BAB",
     "#358221",
     "#A7D282",
     "#87D19E",
     "#FFDB5C",
     "#EECFA8",
     "#ED022A",
     "#EDE9EA",
     "#F2FAFF",
     "#C8C8C8"
   ]};


var clip_2017= collection.mosaic().clip(table);



Map.addLayer(clip_2017,{min:1, max:10, palette: dict['colors']},'clip')
Map.centerObject(table, 10)


Export.image.toDrive({
image : clip_2017,
description : 'Djougou-2017',
folder: "Image",
region:table,
scale : 10,
maxPixels: 1E13,
});