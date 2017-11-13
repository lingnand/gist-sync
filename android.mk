.PHONY: configure build debug clean newbuild

newbuild: clean configure build

# if inside the nixos container
ifneq ("$(wildcard /nix/store)","")

configure:
	cabal configure --with-ghc=arm-unknown-linux-androideabi-ghc --with-ld=arm-linux-androideabi-ld.gold --with-ghc-pkg=arm-unknown-linux-androideabi-ghc-pkg -f target-android && \
	cp -rv dist /target/
build:
	cabal -v build && \
	cp -rv dist /target/ && \
	mv -v dist/build/gist-sync/gist-sync proj.android-studio/app/jni/libgist_sync.so && \
	ndk-build -C proj.android-studio/app NDK_TOOLCHAIN_VERSION=4.9 && \
	{ mkdir -p /target/proj.android-studio/app/libs/armeabi 2>/dev/null || true; } && \
	cp -v proj.android-studio/app/libs/armeabi/* /target/proj.android-studio/app/libs/armeabi/
clean:
	cabal clean

# else we should make use of the docker container
else

# need to have ANDROID_HOME set to a correct sdk location
ifndef ANDROID_HOME
$(error ANDROID_HOME is not set; must be pointing to a valid android sdk)
endif

configure:
	docker build -t nix-cross-android -f Dockerfile.nix-cross-android . && \
	docker run --rm -v `pwd`:/target nix-cross-android configure
build:
	docker build -t nix-cross-android -f Dockerfile.nix-cross-android . && \
	docker run --rm -v `pwd`:/target nix-cross-android build
debug: build
	adb push proj.android-studio/app/libs/armeabi/. /data/local/tmp/gist-sync && \
	adb shell /data/local/tmp/gist-sync/gist-sync ${ARGS}
clean:
	rm -rf dist; docker rmi nix-cross-android 2>/dev/null || true

endif