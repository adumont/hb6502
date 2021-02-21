BOARD = SparkFun:avr:promicro:cpu=16MHzatmega32U4

LIBRARIES := ./libraries

include ../Arduino.mk

USBP := $(wildcard /dev/ttyACM? )

# flash: $(BUILD_PATH)/$(INO_FILE).hex
#   $(ARDUINO_DIR)/hardware/tools/avr/bin/avrdude \
#     -C$(ARDUINO_DIR)/hardware/tools/avr/etc/avrdude.conf \
#     -v -patmega32u4 -cavr109 -P$(USBP) -b57600 -D -Uflash:w:$(<):i

flash: $(BUILD_PATH)/$(INO_FILE).hex
	./reset.py $(USBP) && \
		sleep 2 && \
		$(AVR_DIR)/bin/avrdude -C$(AVR_DIR)/etc/avrdude.conf -v -patmega32u4 -cavr109 -P$(USBP) -b57600 -D -Uflash:w:$(<):i
