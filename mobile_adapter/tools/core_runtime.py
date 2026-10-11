"""Pinned MBC30 core locations shared by launchers and integration tests."""
from pathlib import Path

CORE_REVISION = 'ef247c6545c61358cd8679a78c95c2e92daae13d'
CORE_SHA256 = 'e26383c5151d68716e973c4fd540b43b7c476d267f4078058a0f7bdb5d28e96d'
SOURCE_SHA256 = '61d92544eb0bd22a4b52802505c5fcd597bffdf689710d1f05d2b82d8a0d6865'
CPU_SHA256 = '615ed642810a6ba87f460bf7f186a209eb1ecb96ba547333ac62579f6bf10571'
CORE_HOME = Path('/Volumes/Storage/Cache/mobileadapter/mbc30-ef247c6')
CORE_SOURCE = CORE_HOME / 'doublecherryGB-libretro-ef247c6'
RELEASE_CORE = CORE_HOME / 'DoubleCherryGB-mbc30-macos-universal/DoubleCherryGB_libretro.dylib'
OBSERVED_CORE = CORE_HOME / 'build-observed/DoubleCherryGB_libretro.dylib'
PROFILE_CORE = CORE_HOME / 'build-profile/DoubleCherryGB_libretro.dylib'
