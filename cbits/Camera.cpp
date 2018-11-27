#include <pylon/PylonIncludes.h>
#include <stdlib.h>
#include <stdio.h>

using namespace Pylon;
using namespace std;

#include <pylon/usb/BaslerUsbInstantCamera.h>
typedef Pylon::CBaslerUsbInstantCamera Camera_t;

static int maxNumBuffer = 50;

static int initialize() {
    PylonInitialize();
    return 0;
}

static int dog = initialize();

class Camera {
public :
    CGrabResultPtr* ptrGrabResult;
    int lastNumQueuedBuffers;
    bool notStarted;
    EGrabStrategy strategy;
    Camera_t* camera;

    Camera() {
        CDeviceInfo info;
        info.SetDeviceClass(Camera_t::DeviceClass());
        CTlFactory& tlFactory = CTlFactory::GetInstance();
        DeviceInfoList_t devices;
        if (tlFactory.EnumerateDevices(devices) == 0) {
            std::cerr << "No camera connected" << std::endl;
            throw RUNTIME_EXCEPTION("No Camera Connected");
        }
        Pylon::IPylonDevice* x = CTlFactory::GetInstance().CreateFirstDevice(info);
        camera = new Camera_t(x);
        std::cout << camera->GetDeviceInfo().GetSerialNumber() << std::endl;
      camera->Open();
      camera->MaxNumBuffer = maxNumBuffer;
      lastNumQueuedBuffers = 0;
      strategy = GrabStrategy_OneByOne;
      ptrGrabResult = new CGrabResultPtr();
      notStarted = true;
    }

    ~Camera() {
        camera->StopGrabbing();
        delete ptrGrabResult;
        delete camera;
    }

    void setStrategyOneByOne() {
      camera->StopGrabbing();
      strategy = GrabStrategy_OneByOne;
      notStarted = true;
    }

    void setStrategyLatestImageOnly() {
      camera->StopGrabbing();
      strategy = GrabStrategy_LatestImageOnly;
      notStarted = true;
    }

    uint32_t getHeight() {
        return camera->Height.GetValue();
    }

    uint32_t getWidth() {
        return camera->Width.GetValue();
    }

    String_t getSerial() {
        camera->GetDeviceInfo().GetSerialNumber();
    }

    uint8_t* grab() {
        try {
            if (notStarted) {
                camera->StartGrabbing(strategy);
                notStarted = false;
            }
            camera->RetrieveResult(0, *ptrGrabResult, TimeoutHandling_Return);
            int numQueuedBuffers = camera->NumQueuedBuffers.GetValue();
            if (numQueuedBuffers >= maxNumBuffer - 1) {
                if (lastNumQueuedBuffers < maxNumBuffer - 1) {
                    cerr << "OK" << endl;
                }
            } else if (numQueuedBuffers < lastNumQueuedBuffers) {
                cerr << numQueuedBuffers << "/" << maxNumBuffer << endl;
            }
            if (numQueuedBuffers <= 1) {
                cerr << "Lost frames" << endl;
            }
            lastNumQueuedBuffers = numQueuedBuffers;
            if ((*ptrGrabResult) != 0) {
                if ((*ptrGrabResult)->GrabSucceeded()) {
                    return (uint8_t*) (*ptrGrabResult)->GetBuffer();
                }
                cerr << "Error: " << (*ptrGrabResult)->GetErrorCode() << " "
                     << (*ptrGrabResult)->GetErrorDescription() << endl;
            }
        } catch (GenICam::GenericException &e) {
            cerr << "Caught exception " << e.GetDescription() << endl;
        } catch (...) {
            cerr << "caught " << endl;
        }
        return NULL;
    }
};

extern "C" uint32_t Camera_height(Camera* camera) {
    return camera->getHeight();
}

extern "C" uint32_t Camera_width(Camera* camera) {
    return camera->getWidth();
}

extern "C" String_t Camera_serial(Camera* camera) {
    return camera->getSerial();
}

extern "C" void Camera_setStrategyOneByOne(Camera* camera) {
  camera->setStrategyOneByOne();
}

extern "C" void Camera_setStrategyLatestImageOnly(Camera* camera) {
  camera->setStrategyLatestImageOnly();
}

extern "C" Camera* Camera_new() {
  Camera* camera;
  try {
    camera = new Camera();
    return camera;
  } catch (...) {
    return NULL;
  }
}

extern "C" void Camera_delete(Camera* camera) {
    delete camera;
}

extern "C" uint8_t* Camera_grab(Camera* camera) {
    return camera->grab();
}
