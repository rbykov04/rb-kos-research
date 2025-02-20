# Hello World (hello) example
Example of a minimal possible program.

For details, please refer to the
[documentation](https://click.kaspersky.com/?hl=en-us&link=online_help&pid=kos&version=1.3&customization=KCE&helpid=appendix_hello_example).

# Prerequisites
Before you begin, ensure that you have met the following requirements:
- You have installed the latest version of [KasperskyOS Community Edition](https://os.kaspersky.com/development/download/).
- You have Ubuntu 22.04 LTS "Jammy Jellyfish" x64.

# Usage
## QEMU
To build the example, execute the following command:
```
$ ./cross-build.sh
```
The `cross-build.sh` script both builds the example on QEMU and runs it.

## Hardware
1. To build the example, execute the following command:
   ```
   $ ./cross-build.sh --target {kos-image|sd-image}
   ```
   where:
   * `kos-image` creates a KasperskyOS-based solution image that includes the example;
   * `sd-image` creates a file system image for a bootable SD card.
2. [Prepare the hardware and a bootable SD card to run the example.](https://click.kaspersky.com/?hl=en-us&link=online_help&pid=kos&version=1.3&customization=KCE&helpid=building_and_running_sample_programs).
3. [Run the example.](https://click.kaspersky.com/?hl=en-us&link=online_help&pid=kos&version=1.3&customization=KCE&helpid=running_sample_programs_rpi).

© 2024 AO Kaspersky Lab
