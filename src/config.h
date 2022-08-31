#ifndef CONFIG_H_
#define CONFIG_H_

typedef enum Platform {
  PLATFORM_LINUX_x86_64 = 0,
  PLATFORM_COUNT,
} Platform;

typedef struct Config {
  Platform platform;
} Config;

extern Config config;

#endif // CONFIG_H_