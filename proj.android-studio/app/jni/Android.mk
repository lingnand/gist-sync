LOCAL_PATH := $(call my-dir)
include $(CLEAR_VARS)

LOCAL_MODULE := gist_sync
LOCAL_SRC_FILES := main.cpp

LOCAL_C_INCLUDES := $(LOCAL_PATH)/../../../cbits
# we are cross-compiling against 7.10.2
LOCAL_C_INCLUDES += $(LOCAL_PATH)/../../../cbits/ghc-7.10.2

LOCAL_SHARED_LIBRARIES := haskell_shared

# need 8 dollar signs because..maybe secondary expansion in make?
LOCAL_LDFLAGS += -Wl,-rpath,'$$$$$$$$ORIGIN'

include $(BUILD_EXECUTABLE)

#================================

include $(CLEAR_VARS)
LOCAL_MODULE := haskell_shared
LOCAL_SRC_FILES := libgist_sync.so
include $(PREBUILT_SHARED_LIBRARY)