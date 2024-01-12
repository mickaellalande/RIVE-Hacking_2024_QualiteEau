# https://code.earthengine.google.com/tasks
import pandas as pd
import ee
import io
import requests
import os.path

# According to this site : http://www.csgnetwork.com/degreelenllavcalc.html
# At 47N, 1 degree of latitude  = 111170.81m
# 1 degree of longitude = 76055.97m
# Here's how many degrees we need to add for a 1 km buffer around sites
longitude_buffer = 1000 / 76055.97
latitude_buffer = 1000 / 111170.81

points = pd.read_csv("donnees/subset_prototype.csv")

# points = pd.DataFrame(data={
#   'ID': ["TR", "Shawi"], 
#   'latitude': [46.364774572473756, 46.46656664221568],
#   'longitude': [-72.58160482640592, -72.68107808419225]
# })
# points

ee.Initialize()

# https://developers.google.com/earth-engine/datasets/catalog/COPERNICUS_S2_SR#colab-python
def mask_s2_clouds(image):
  """Masks clouds in a Sentinel-2 image using the QA band.

  Args:
      image (ee.Image): A Sentinel-2 image.

  Returns:
      ee.Image: A cloud-masked Sentinel-2 image.
  """
  qa = image.select('QA60')

  # Bits 10 and 11 are clouds and cirrus, respectively.
  cloud_bit_mask = 1 << 10
  cirrus_bit_mask = 1 << 11

  # Both flags should be set to zero, indicating clear conditions.
  mask = (
      qa.bitwiseAnd(cloud_bit_mask)
      .eq(0)
      .And(qa.bitwiseAnd(cirrus_bit_mask).eq(0))
  )

  return image.updateMask(mask).divide(10000)

for index, point in points.iterrows() :

  file_name = "grosses_donnees/thumbs/"+point.ID+'.png'
  
  # On travaille uniquement si on a pas déjà le fichier...
  if (os.path.isfile(file_name)) :
    continue

  aoi = ee.Geometry.Rectangle(
    point.longitude-longitude_buffer,
    point.latitude-latitude_buffer,
    point.longitude+longitude_buffer,
    point.latitude+latitude_buffer
  )
  
  c = (ee.ImageCollection('COPERNICUS/S2')
  #.filterDate('2022-01-01','2022-12-31')
  .filterDate(str(point.annee)+'-01-01',str(point.annee)+'-12-31')
  .filterBounds(aoi)
  .filter(ee.Filter.dayOfYear(152, 273)) # 1er juin au 30 septembre
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20))
  .map(mask_s2_clouds)
  )
  
  # task = ee.batch.Export.image.toDrive(
  #   c.median(),
  #   scale= 10, # Les bandes visibles ont des pixels de 10m sur Sentinel 2
  #   region = aoi,
  #   description= point.ID
  # )
  # task.start()

  # https://developers.google.com/earth-engine/apidocs/ee-image-getdownloadurl
  # url = c.median().getDownloadUrl({
  #     'region': aoi,
  #     'scale': 10,
  #     'format': 'GEO_TIFF'
  # })
  
  url = c.median().getThumbUrl({
      'region': aoi,
      'scale': 10,
      'format': 'png',
      'bands': ['B4', 'B3', 'B2'],
      'min' : [0,0,0],
      'max' : [1,1,1]
  })
  
  response = requests.get(url)
  with open(file_name, 'wb') as fd:
    fd.write(response.content)
