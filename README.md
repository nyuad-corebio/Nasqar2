# Nasqar2

### Install locally with Docker
Official Nasqar2 image is hosted in Dockerhub. Run Nasqar2 in a Docker container and access it at http://localhost:80.

Make sure Docker software is up and running. 

```
docker run -p 80:3232 nyuadcorebio/nasqarall:nasqar
```

- If you run this service on a server, specify the (IP-address or hostname):80 on the browser.
- If you run this service on a standalone machine (e.g. laptop), specify localhost:80 on the browser.

To run Nasqar2 on another port. for eg:- 8080

```
docker run -p 8080:3232 nyuadcorebio/nasqarall:nasqar
```
It can be access via http://localhost:8080

### Build a local image with Docker ( Optional )
If you want to customize the code and then build the docker image. Refer to below instructions. 


Clone the git repository to your working directory.
```
git clone https://github.com/nyuad-corebio/Nasqar2/
cd Nasqar2/
sh download_data.sh
```

Build the docker image as follows:- 
Note:- Make sure you have sufficient space. It will be around 10GB and takes an hour to finish. 
```
docker build  --progress=plain  -t <specify-image-name> .
```

Verify the build image
```
docker image ls 
```

To start Nasqar2 using the build image.
```
docker run --name <specify-name-of-container>  -p 80:3232 -it <specify-image-name>
```
It can be access via http://localhost:80


