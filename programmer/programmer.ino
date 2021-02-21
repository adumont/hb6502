#define DATA        9
#define SHIFT      10
#define LATCH      16

char data_pins[] = { 6, 7, 8, 15, 18, 19, 20, 21 };

#define WRITE_EN   14

#include "SerialCommand.h"

SerialCommand sCmd;     // The demo SerialCommand object

void pulse(int pin) {
  digitalWrite(pin, LOW);
  digitalWrite(pin, HIGH);
  digitalWrite(pin, LOW);
}

void setAddress(int address, bool outputEnable) {
  shiftOut(DATA, SHIFT, MSBFIRST, (address >> 8) | (outputEnable ? 0x00 : 0x80));
  shiftOut(DATA, SHIFT, MSBFIRST, address);

  pulse(LATCH);
}

void setup() {
  pinMode(DATA, OUTPUT);
  pinMode(SHIFT, OUTPUT);
  pinMode(LATCH, OUTPUT);

  digitalWrite(WRITE_EN, HIGH);
  pinMode(WRITE_EN, OUTPUT);

  Serial.begin(115200);

  sCmd.addCommand("s", shift_cmd);
  sCmd.addCommand("set",  set_cmd);
  sCmd.setDefaultHandler(unrecognized_cmd);      // Handler for command that isn't matched  (says "What?")

  for( char i=0; i<8; i++ ) {
    pinMode(data_pins[i], OUTPUT);
  }
}

void loop() {
  sCmd.readSerial();
}

void shift_cmd() {
  // s [0|1]
  // shift a digit through the shift registers
  int aNumber = 0;
  char *arg;

  arg = sCmd.next();

  if (arg != NULL) {
    aNumber = (atoi(arg) == 0) ? 0 : 1 ;
  }

  Serial.print("shift ");
  Serial.println(aNumber);

  if(aNumber == 0) {
    digitalWrite(DATA, LOW);
  } else {
    digitalWrite(DATA, HIGH);
  }
  pulse(SHIFT);
  delay(10);
  pulse(LATCH);
  delay(10);
}

void set_cmd() {
  int addr = 0;
  int oe = 0;

  char *arg;

  arg = sCmd.next();
  if (arg != NULL) {
    addr = atoi(arg);
  }

  arg = sCmd.next();
  if (arg != NULL) {
    oe = (atoi(arg) == 0) ? 0 : 1;
  }

  Serial.print("set addr ");
  Serial.print(addr);
  Serial.print(" ");
  Serial.println(oe);
  setAddress(addr, (oe == 0)?false:true);

}

void unrecognized_cmd(const char *command) {
  Serial.println("What?");
}
