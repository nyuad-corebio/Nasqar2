# Nasqar2

### Install locally with Docker
Official Nasqa2 image is here and it can be downloaded with docker pull. Run Nasqar2 in a Docker container and access it at http://localhost:8080.

docker pull nyuadcorebio/nasqarall:nasqar
docker run --name tsar_dev --rm -p 8080:3232 -it tsar2_dev_1


### Build a local image with Docker
If you want to build a local image, run:

sh download_data.sh
docker build  --progress=plain  -t tsar2_dev_1 .
docker run --name tsar_dev --rm -p 8080:3232 -it tsar2_dev_1



