# geospatial_rocker.sh

sudo docker pull rocker/geospatial
sudo docker run -e PASSWORD=yourpassword --rm -p 8787:8787 rocker/geospatial
