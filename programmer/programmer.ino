/* Pin assignments
 *
 * These pin assigments correspond to these port mappings on the Atmega32u4
 *
 * AT28C256 EEPROM
 *   D0   PORTD, bit 7   Arduino pin  6 (digital  6)
 *   D1   PORTE, bit 6   Arduino pin  7 (digital  7)
 *   D2   PORTB, bit 4   Arduino pin  8 (digital  8)
 *   D3   PORTB, bit 1   Arduino pin 15 (digital 15)
 *   D4   PORTF, bit 7   Arduino pin 18 (analog  A0)
 *   D5   PORTF, bit 6   Arduino pin 19 (analog  A1)
 *   D6   PORTF, bit 5   Arduino pin 20 (analog  A2)
 *   D7   PORTF, bit 4   Arduino pin 21 (analog  A3)
 * 
 *   WE#  PORTB, bit 3   Arduino pin 14 (miso)
 *   OE#  PORTD, bit 4   Arduino pin  4 (digital  4)
 *   CE#  PORTC, bit 6   Arduino pin  5 (digital  5)
 */


// some bit manipulation macros
#define SET(x,y) x |= (1 << y)
#define CLEAR(x,y) x &= ~(1<< y)
#define TOGGLE(x,y) (x ^= (1<<y))
#define READ(x,y) ((0u == (x & (1<<y)))?0u:1u)
#define BITN(num, n) (((num) >> (n)) & 1)

// artifice to use the pins macros in previous bit manipulation macros
// see https://stackoverflow.com/questions/64630809/how-to-define-a-macro-of-two-tokens-in-cpp/
#define _SET(x)    SET(x)
#define _CLEAR(x)  CLEAR(x)
#define _TOGGLE(x) TOGGLE(x)
#define _READ(x)   READ(x)

// define the pins

// EEPROM
#define WE_PIN   PORTB,3
#define OE_PIN   PORTD,4
#define CE_PIN   PORTC,6

char data_pins[] = { 6, 7, 8, 15, 18, 19, 20, 21 };


// Shift Register
#define DATA        9
#define SHIFT      10
#define LATCH      16

int data_pin_mode = 1234; // dummy value


// WE# 
#define WE_LOW  _CLEAR(WE_PIN)
#define WE_HIGH _SET(WE_PIN)

// CE# 
#define CE_LOW  _CLEAR(CE_PIN)
#define CE_HIGH _SET(CE_PIN)

// OE# 
#define OE_LOW  _CLEAR(OE_PIN)
#define OE_HIGH _SET(OE_PIN)


#include "src/SerialCommand.h"
#include "src/hextools.c"

#define SERIAL_TIMEOUT 10000

SerialCommand sCmd;

void pulse(int pin) {
  digitalWrite(pin, LOW);
  digitalWrite(pin, HIGH);
  digitalWrite(pin, LOW);
}

void setAddress(uint32_t address, bool outputEnable) {
  //shiftOut(DATA, SHIFT, MSBFIRST, (address >> 8) | ( bank << 6 )  | (outputEnable ? 0x00 : 0x80));
  shiftOut(DATA, SHIFT, MSBFIRST, (address >> 8) | (outputEnable ? 0x00 : 0x80));
  shiftOut(DATA, SHIFT, MSBFIRST, address);

  pulse(LATCH);
}

void setDataBusMode(int mode) {
  if(data_pin_mode == mode)
    return;
    
  for( char i=0; i<8; i++ ) {
    pinMode(data_pins[i], mode);
  }
  data_pin_mode = mode;
}

byte readDATA() {
  setDataBusMode(INPUT);

  byte data = 0;
  for (int i = 7; i >= 0; i--) {
    data = (data << 1) + digitalRead(data_pins[i]);
  }
  return data;  
}

byte readEEPROM(uint32_t address) {
  // set data pin as input (we read)
  setDataBusMode(INPUT);

  setAddress(address, true); // oe=true aka, we read

  CE_LOW;
  _delay_loop_1(1);
  OE_LOW;
  _delay_loop_1(1);

  uint8_t data = readDATA();

  OE_HIGH;
  _delay_loop_1(1);
  CE_HIGH;
  _delay_loop_1(1);

  return data;
}

void setData(byte data) {
  setDataBusMode(OUTPUT);

  // for (int i = 0; i <= 7; i++) {
  //   digitalWrite(data_pins[i], data & 1);
  //   data = data >> 1;
  // }
  PORTD = (PORTD & B01111111) | ( BITN(data, 0) << 7); // put data bit 0 into port D bit 7
  PORTE = (PORTE & B10111111) | ( BITN(data, 1) << 6);
  PORTB = (PORTB & B11101101) | ( BITN(data, 2) << 4) 
                              | ( BITN(data, 3) << 1);
  PORTF = (PORTF & B00001111) | ( BITN(data, 4) << 7)
                              | ( BITN(data, 5) << 6)
                              | ( BITN(data, 6) << 5)
                              | ( BITN(data, 7) << 4);
}

void writeEEPROM(uint32_t address, byte data) {
  // set data pin as input (we read)
  setDataBusMode(OUTPUT);

  setAddress(address, false);
  OE_HIGH;
  _delay_loop_1(1);
  CE_LOW;
  
  setData(data);

  _delay_loop_1(1);
  WE_LOW;
  _delay_loop_1(1);
  WE_HIGH;
  _delay_loop_1(1);

  CE_HIGH;
  OE_HIGH;
  _delay_loop_1(1);

  delay(10);
}

void setup() {
  setDataBusMode(INPUT);
  pinMode(DATA, OUTPUT);
  pinMode(SHIFT, OUTPUT);
  pinMode(LATCH, OUTPUT);

  WE_HIGH;
  CE_HIGH;
  OE_HIGH;
  pinMode(14, OUTPUT);   // then we set as output
  pinMode(4, OUTPUT);    // then we set as output
  pinMode(5, OUTPUT);    // then we set as output

  OE_LOW;

  Serial.begin(115200);
  while(!Serial);
  Serial.println("EEPROM Programmer Ready");

  sCmd.addCommand("s", shift_cmd);  // shift a byte into the shift register and latch it
  sCmd.addCommand("set",  set_cmd); // 
  sCmd.addCommand("r",  read_cmd);
  sCmd.addCommand("w",  write_cmd);
  sCmd.addCommand("d",  dump_cmd);
  sCmd.addCommand("erase", erase_cmd);
  sCmd.addCommand("test",  test_cmd);
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

// set address to arg (in hex)
void set_cmd() {
  uint32_t addr = parse_cmd_hex32(0);
  bool oe = 0;

  char buf[10];
  Serial.print("set addr ");
  sprintf(buf, "%04x:  ", addr);
  Serial.println(buf);
  setAddress(addr,oe);
}

void write_cmd() {
  uint32_t addr = parse_cmd_hex32(0);
  byte data = (byte)(parse_cmd_hex32(0));

  Serial.println(addr);
  Serial.println(data);

  char buf[256];
  sprintf(buf, "write addr %04x: %04x", addr, data);
  Serial.println(buf);
  sprintf(buf, "write addr %04x: %04x", data, addr );
  Serial.println(buf);
  sprintf(buf, "write addr %02x: %02x", addr, data);
  Serial.println(buf);
  sprintf(buf, "write addr %02x: %02x", data, addr );
  Serial.println(buf);
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
  uint32_t start = parse_cmd_hex32(0);
  int pages = parse_cmd_int(0);

  dump(start, pages);

}

// binary dump
void bdump_cmd() {
  uint32_t start_addr = parse_cmd_hex32(0);
  uint32_t end_addr = parse_cmd_hex32(0);

  for(uint32_t i = start_addr; i<=end_addr; i++) {
    Serial.write( readEEPROM(i) );
  }

}

int read_serial_byte(void)
{
  unsigned long start = millis();
  while (!Serial.available()) {
    if (millis() - start >= SERIAL_TIMEOUT) {
      return -1;
    }
  }
  return Serial.read();
}

uint32_t parse_cmd_hex32(uint32_t defaultValue) {
  char *arg;

  arg = sCmd.next();
  if (arg != NULL) {
    return getHex32(arg, defaultValue);
  } else {
    return defaultValue;
  }
}

uint32_t parse_cmd_int(uint32_t defaultValue) {
  char *arg;

  arg = sCmd.next();
  if (arg != NULL) {
    return atoi(arg);
  } else {
    return defaultValue;
  }
}

// flash to eeprom from binary feed
void bflash_cmd() {
  uint32_t start_addr = parse_cmd_hex32(0);
  uint32_t end_addr = parse_cmd_hex32(0);

  int b;

  Serial.println("Receiving bytes");
  for(uint32_t i = start_addr; i<=end_addr; i++) {
    b = read_serial_byte();
    if(b==-1) {
      Serial.println("ERROR: timeout getting data");
      return;
    }
    writeEEPROM(i, b);
  }
  Serial.println("Done");

}

void read_cmd() {
  uint32_t addr = parse_cmd_hex32(0);

  Serial.print("Read addr ");
  Serial.print(addr);
  Serial.print(" = ");
  Serial.println(readEEPROM(addr));

}

void unrecognized_cmd(const char *command) {
  Serial.println("What?");
}

void test_cmd() {
  char buf[256];
  byte d;

  Serial.println("Next step: press 'n'");

  Serial.println("Pin 1 (A14)");
  while(Serial.read()!='n') {
    setAddress(1<<14, 0);
    delay(250);
    setAddress(0, 0);
    delay(250);
  }

  CE_LOW;
  Serial.println("CE Low");
  while(Serial.read()!='n');

  CE_HIGH;
  Serial.println("CE High");
  while(Serial.read()!='n');

  OE_LOW;
  Serial.println("OE Low");
  while(Serial.read()!='n');

  OE_HIGH;
  Serial.println("OE High");
  while(Serial.read()!='n');

  WE_LOW;
  Serial.println("WE Low");
  while(Serial.read()!='n');

  WE_HIGH;
  Serial.println("WE High");
  while(Serial.read()!='n');


  Serial.println("Moving 1 on address bus");
  for(char i = 0; i<15; i++) {
    d=1<<i;

    setAddress(d, 0);

    sprintf(buf, "  address bit %d is 1 : %d", i, d);
    Serial.println(buf);

    while(Serial.read()!='n');
  }

  Serial.println("Moving 0 on data bus");
  for(char i = 0; i<15; i++) {
    d=~(1<<i);

    setAddress(d, 0);

    sprintf(buf, "  address bit %d is 0 : %d", i, d);
    Serial.println(buf);

    while(Serial.read()!='n');
  }
  
  Serial.println("Moving 1 on data bus");
  for(char i = 0; i<8; i++) {
    d=1<<i;

    setData(d);

    sprintf(buf, "  data bit %d is 1 : %d", i, d);
    Serial.println(buf);

    while(Serial.read()!='n');
  }

  Serial.println("  Test 2 - Moving 0 on data bus");
  for(char i = 0; i<8; i++) {
    d=~(1<<i);

    setData(d);

    sprintf(buf, "data bit %d is 0 : %d", i, d);
    Serial.println(buf);

    while(Serial.read()!='n');
  }

}