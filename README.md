# hpylon

Tested on Ubuntu 18.04 with [acA640-750um](https://www.baslerweb.com/en/products/cameras/area-scan-cameras/ace/aca640-750um/) and [acA640-750uc](https://www.baslerweb.com/en/products/cameras/area-scan-cameras/ace/aca640-750uc/) running at native ```640x480@751fps``` and cropped ```640x120@2177fps```

## Instructions

Install [debian package](https://www.baslerweb.com/en/sales-support/downloads/software-downloads/pylon-5-1-0-linux-x86-64-bit-debian/), fill out stupid survey.

Add pylon libraries with 
```bash
echo "/opt/pylon5/lib64" | sudo tee /etc/ld.so.conf.d/pylon.conf
sudo ldconfig
```
Then in repo directory
```stack install```

## Usage

This library only exposes the function 
```haskell 
cameraProducer :: IO (Maybe (SerialNumber, DIM2, Producer (S.Vector Word8) IO ()))
```
This provides a [pipe](https://hackage.haskell.org/package/pipes), producing Storable Vectors for each call to 
```haskell 
await
```

see ```app/Main.hs``` for an example
