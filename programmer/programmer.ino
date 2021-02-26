#define DATA        9
#define SHIFT      10
#define LATCH      16

char data_pins[] = { 6, 7, 8, 15, 18, 19, 20, 21 };

int data_pin_mode = 1234; // dummy value

#define WRITE_EN   14

#include "src/SerialCommand.h"

SerialCommand sCmd;

void pulse(int pin) {
  digitalWrite(pin, LOW);
  digitalWrite(pin, HIGH);
  digitalWrite(pin, LOW);
}

void setAddress(int address, bool outputEnable) {
  //shiftOut(DATA, SHIFT, MSBFIRST, (address >> 8) | ( bank << 6 )  | (outputEnable ? 0x00 : 0x80));
  shiftOut(DATA, SHIFT, MSBFIRST, (address >> 8) | (outputEnable ? 0x00 : 0x80));
  shiftOut(DATA, SHIFT, MSBFIRST, address);

  pulse(LATCH);
}

byte readEEPROM(unsigned int address) {
  // set data pin as input (we read)
  setDataPinMode(INPUT);

  setAddress(address, true); // oe=true aka, we read

  _delay_loop_1(1); // ~188ns @ 16MHz

  byte data = 0;
  for (int i = 7; i >= 0; i--) {
    data = (data << 1) + digitalRead(data_pins[i]);
  }
  
  return data;
}

void setDataPinMode(int mode) {
  if(data_pin_mode == mode)
    return;
    
  for( char i=0; i<8; i++ ) {
    pinMode(data_pins[i], mode);
  }
  data_pin_mode = mode;
}

void writeEEPROM(unsigned int address, byte data) {
  // set data pin as input (we read)
  setDataPinMode(OUTPUT);

  setAddress(address, false); // oe=false aka, we will write

  for (int i = 0; i <= 7; i++) {
    digitalWrite(data_pins[i], data & 1);
    data = data >> 1;
  }  

  digitalWrite(WRITE_EN, LOW);
  delayMicroseconds(1);
  //_delay_loop_1(1); // ~188ns @ 16MHz
  digitalWrite(WRITE_EN, HIGH);
  delay(10);
}


void setup() {
  setDataPinMode(INPUT);
  pinMode(DATA, OUTPUT);
  pinMode(SHIFT, OUTPUT);
  pinMode(LATCH, OUTPUT);

  digitalWrite(WRITE_EN, HIGH); // we first set HIGH (no write),
  pinMode(WRITE_EN, OUTPUT);    // then we set as output

  Serial.begin(115200);
  while(!Serial);
  Serial.println("EEPROM Programmer Ready");

  sCmd.addCommand("s", shift_cmd);
  sCmd.addCommand("set",  set_cmd);
  sCmd.addCommand("r",  read_cmd);
  sCmd.addCommand("w",  write_cmd);
  sCmd.addCommand("d",  dump_cmd);
  sCmd.addCommand("erase",  erase_cmd);
  sCmd.setDefaultHandler(unrecognized_cmd);      // Handler for command that isn't matched  (says "What?")
}

void loop() {
  sCmd.readSerial();
}

void shift_cmd() {
  // s [0|1]
  // shift a digit through the shift registers
  unsigned int aNumber = 0;
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
  unsigned int addr = 0;
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

void write_cmd() {
  unsigned int addr = 0;
  byte data = 0;

  char *arg;

  arg = sCmd.next();
  if (arg != NULL) {
    addr = atoi(arg);
  }

  arg = sCmd.next();
  if (arg != NULL) {
    data = (byte)(atoi(arg));
  }

  Serial.print("write addr ");
  Serial.print(addr);
  Serial.print(" ");
  Serial.println(data);
  writeEEPROM(addr, data);
}

void dump(unsigned int start, int pages) {
  char buf[10];
  unsigned int addr_end = start + 256*pages - 1;
  //for(unsigned int a = 0; a<=32767; a++) {
  for(unsigned int a = start; a<=addr_end ; a++) {
    if( a % 256 == 0 ) {
      Serial.println();
    }
    if( a % 16 == 0 ) {
      // new line and print address
      sprintf(buf, "%04x:  ", a);
      Serial.print(buf);
    }
    if( a % 16 == 8 ) {
      // middle of line
      Serial.print("  ");
    }
    sprintf(buf, "%02x ", readEEPROM(a));
    Serial.print(buf);

    if( a % 16 == 15 ) {
      // end of line
      Serial.println();
    }
  }
}

void erase_cmd() {
  Serial.println("Erasing");
  for(unsigned int a = 0; a<32*1024 ; a++) {
    if( a % 256 == 0 ) {
      Serial.print(".");
    }
    if( a!=0 && (a % (32*256)) == 0 ) {
      Serial.println();
    }
    writeEEPROM(a, 255);
  }
  Serial.println("done");
}

void dump_cmd() {
  unsigned int start = 0;
  int pages = 1;

  char *arg;

  arg = sCmd.next();
  if (arg != NULL) {
    start = atoi(arg);
  }

  arg = sCmd.next();
  if (arg != NULL) {
    pages = atoi(arg);
  }

  dump(start, pages);

}

void read_cmd() {
  unsigned int addr = 0;
  char *arg;

  arg = sCmd.next();
  if (arg != NULL) {
    addr = atoi(arg);
  }

  Serial.print("Read addr ");
  Serial.print(addr);
  Serial.print(" = ");
  Serial.println(readEEPROM(addr));

}

void unrecognized_cmd(const char *command) {
  Serial.println("What?");
}
