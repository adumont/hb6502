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
 * 
 * Shift Registers Pins
 *  DATA  PORTB, bit 5   Arduino pin  9 (digital  9)
 *  SHIFT PORTB, bit 6   Arduino pin 10 (digital 10)
 *  LATCH PORTB, bit 2   Arduino pin 16 (mosi)
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

//#define DEBUG
#undef DEBUG

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

void set_address(uint16_t address) {
  shiftOut(DATA, SHIFT, MSBFIRST, address >> 8);
  shiftOut(DATA, SHIFT, MSBFIRST, address);

  pulse(LATCH);
}

void data_bus_mode(uint8_t mode) {
  if(data_pin_mode == mode)
    return;
    
  for( uint8_t i=0; i<8; i++ ) {
    pinMode(data_pins[i], mode);
  }
  data_pin_mode = mode;
}

byte read_data_bus() {
  data_bus_mode(INPUT);

  return BITN(PINF,4) << 7 |
         BITN(PINF,5) << 6 |
         BITN(PINF,6) << 5 |
         BITN(PINF,7) << 4 |
         BITN(PINB,1) << 3 |
         BITN(PINB,4) << 2 |
         BITN(PINE,6) << 1 |
         BITN(PIND,7) ;
}

void write_data_bus(byte data) {
  data_bus_mode(OUTPUT);

  PORTD = (PORTD & B01111111) | ( BITN(data, 0) << 7); // put data bit 0 into port D bit 7
  PORTE = (PORTE & B10111111) | ( BITN(data, 1) << 6);
  PORTB = (PORTB & B11101101) | ( BITN(data, 2) << 4) 
                              | ( BITN(data, 3) << 1);
  PORTF = (PORTF & B00001111) | ( BITN(data, 4) << 7)
                              | ( BITN(data, 5) << 6)
                              | ( BITN(data, 6) << 5)
                              | ( BITN(data, 7) << 4);
}

uint8_t read_eeprom_byte(uint16_t address) {
  // set data pin as input (we read)
  data_bus_mode(INPUT);

  set_address(address);

  CE_LOW;
  OE_LOW;

  // _delay_loop_1(1);
  delayMicroseconds(1);

  uint8_t data = read_data_bus();

  OE_HIGH;
  CE_HIGH;

  return data;
}

void wait_write_completed(uint8_t data) {
  data_bus_mode(INPUT);

  uint8_t b1, b2; // for D6

  while(1) {
    CE_HIGH;
    OE_HIGH;
    delayMicroseconds(1);
    CE_LOW;
    OE_LOW;
    delayMicroseconds(1);
    b1 = read_data_bus();

    CE_HIGH;
    OE_HIGH;
    delayMicroseconds(1);
    CE_LOW;
    OE_LOW;
    delayMicroseconds(1);
    b2 = read_data_bus();

    if( b1 == b2 && b2 == data ) {
      break;
    }
  }
  CE_HIGH;
  OE_HIGH;  
}

void write_eeprom_byte(uint16_t address, byte data) {
  // set data pin as input (we read)
  OE_HIGH;
  WE_HIGH;

  set_address(address);

  data_bus_mode(OUTPUT);

  write_data_bus(data);

  CE_LOW;

  WE_LOW;

  delayMicroseconds(1);

  WE_HIGH;

  wait_write_completed(data);

  CE_HIGH;
  OE_HIGH;
}

void write_eeprom_page(uint16_t base, uint8_t page[], uint8_t len) {
  // set data pin as input (we read)
  OE_HIGH;
  WE_HIGH;

  data_bus_mode(OUTPUT);

  for(uint8_t offset = 0 ; offset<len; offset++) {
    set_address(base + offset);

    write_data_bus(page[offset]);
    delayMicroseconds(1);

    CE_LOW;
    WE_LOW;
    delayMicroseconds(1);
    CE_HIGH;
    WE_HIGH;

  }

  uint8_t data = page[len-1]; // last byte to be written

  WE_HIGH;

  wait_write_completed(data);

  CE_HIGH;
  OE_HIGH;
}

void setup() {
  data_bus_mode(INPUT);
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

#ifdef DEBUG
  sCmd.addCommand("s", shift_cmd);  // shift a byte into the shift register and latch it
  sCmd.addCommand("set",  set_cmd); // 
  sCmd.addCommand("test",  test_cmd);
#endif
  sCmd.addCommand("r",  read_cmd);
  sCmd.addCommand("w",  write_cmd);
  sCmd.addCommand("d",  dump_cmd);
  sCmd.addCommand("erase", erase_cmd);
  sCmd.addCommand("put", bflash_cmd);
  sCmd.addCommand("get", bdump_cmd);
  sCmd.setDefaultHandler(unrecognized_cmd);      // Handler for command that isn't matched  (says "What?")
  Serial.print(">");
}

void loop() {
  sCmd.readSerial();
}

#ifdef DEBUG
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
  uint16_t addr = parse_cmd_hex32(0);
  bool oe = 0;

  char buf[10];
  Serial.print("set addr ");
  sprintf(buf, "%04x:  ", addr);
  Serial.println(buf);
  set_address(addr);
}
#endif //DEBUG

void write_cmd() {
  uint16_t addr = parse_cmd_hex32(0);
  uint8_t  data = parse_cmd_hex32(0);

  write_eeprom_byte(addr, data);
}

void read_cmd() {
  uint16_t addr = parse_cmd_hex32(0);

  char buf[256];
  sprintf(buf, "%04x: %02x", addr, read_eeprom_byte(addr));
  Serial.println(buf);
}

void txt_dump(uint16_t start, uint16_t pages, uint16_t offset) {
  char buf_hex[41];
  char buf_asc[17];
  buf_asc[16]=0;

  uint8_t b;
  uint16_t a = start;
  for(uint16_t p=0; p<pages; p++) {
    for(uint8_t r=0; r<16; r++) {
      sprintf(buf_hex, "%08x: ", a + offset);
      Serial.print(buf_hex);

      uint8_t i = 0 ; // index in buf

      for(uint8_t c=0; c<16; c++) {

        b=(uint8_t)read_eeprom_byte(a);

        buf_asc[c]=( b<32 || b>126 ) ? '.' : b ;

        buf_hex[i++] = hex[b  >>  4];
        buf_hex[i++] = hex[b & 0x0f];

        if(c % 2 == 1) {
          buf_hex[i++] = ' ';
        }

        a++;
      }
      buf_hex[i] = 0;

      Serial.print(buf_hex);
      Serial.print(" ");
      Serial.println(buf_asc);

    }
    Serial.println("");
  }
}

void erase_cmd() {
  uint8_t data = parse_cmd_hex32(255);

  // fill a dummy 64B page with data
  uint8_t page[64];
  for(uint8_t i=0;i<64;i++){
    page[i]=data;
  }

  Serial.print("Erasing ");
  for(uint16_t a = 0; a<32*1024 ; a+=64) {
    write_eeprom_page(a, page, 64);
    Serial.print(".");
  }
  Serial.println("done");
}

void dump_cmd() {
  uint16_t start = parse_cmd_hex32(0);
  uint16_t pages = parse_cmd_int(1);
  uint16_t offset = parse_cmd_hex32(0);

  if( start > 0x7f00 ) {
    start=0x7f00;
  }

  start = (start / 256) * 256;

  if( pages == 0 ) {
    pages = 1;
  }

  txt_dump(start, pages, offset);

}

// binary dump
void bdump_cmd() {
  uint16_t start_addr = parse_cmd_hex32(0);
  uint16_t len = parse_cmd_int(0);

  if(len==0){
    Serial.println("ERROR: length can't be 0");
  }

  for(uint16_t i = start_addr; i<start_addr+len; i++) {
    Serial.write( read_eeprom_byte(i) );
  }

  Serial.println("Done");

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

uint16_t parse_cmd_hex32(uint16_t defaultValue) {
  char *arg;

  arg = sCmd.next();
  if (arg != NULL) {
    return getHex32(arg, defaultValue);
  } else {
    return defaultValue;
  }
}

uint16_t parse_cmd_int(uint16_t defaultValue) {
  char *arg;

  arg = sCmd.next();
  if (arg != NULL) {
    return atoi(arg);
  } else {
    return defaultValue;
  }

}

// PUT ADDR (hex) LENGTH (dec)
// flash to eeprom from binary feed
void bflash_cmd() {
  uint16_t start_addr = parse_cmd_hex32(0);
  uint16_t len = parse_cmd_int(0);

  uint16_t page_start, block_len;

  uint16_t end_addr = start_addr + len - 1;

  uint8_t page[64]; // byte array to hold a page

  uint8_t d = start_addr % 64;
  int b;

  char buf[80]; 

  // Serial.println("GO - receiving data...");

  while(len>0) {
    block_len = 64 - ( start_addr % 64 );
    if ( len < block_len ) {
      block_len = len;
    }

    // sprintf(buf, "  start_addr: $%04x, block_len : %02d ...", start_addr, block_len); Serial.print(buf);

    for(uint8_t i = 0; i<block_len; i++) {
      b = read_serial_byte();
      if(b==-1) {
        Serial.println("ERROR: timeout getting data");
        return;
      }
      page[i]=b;
    }

    write_eeprom_page(start_addr, page, block_len);

    // Serial.println("OK: page written");

    start_addr += block_len;
    len -= block_len;
  }

  Serial.println("DONE");

}

void unrecognized_cmd(const char *command) {
  Serial.println("What?");
}

#ifdef DEBUG
void test_cmd() {
  char buf[256];
  byte d;

  Serial.println("Next step: press 'n'");

  Serial.println("Pin 1 (A14)");
  while(Serial.read()!='n') {
    set_address(1<<14);
    delay(250);
    set_address(0);
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

    set_address(d);

    sprintf(buf, "  address bit %d is 1 : %d", i, d);
    Serial.println(buf);

    while(Serial.read()!='n');
  }

  Serial.println("Moving 0 on data bus");
  for(char i = 0; i<15; i++) {
    d=~(1<<i);

    set_address(d);

    sprintf(buf, "  address bit %d is 0 : %d", i, d);
    Serial.println(buf);

    while(Serial.read()!='n');
  }
  
  Serial.println("Moving 1 on data bus");
  for(char i = 0; i<8; i++) {
    d=1<<i;

    write_data_bus(d);

    sprintf(buf, "  data bit %d is 1 : %d", i, d);
    Serial.println(buf);

    while(Serial.read()!='n');
  }

  Serial.println("  Test 2 - Moving 0 on data bus");
  for(char i = 0; i<8; i++) {
    d=~(1<<i);

    write_data_bus(d);

    sprintf(buf, "data bit %d is 0 : %d", i, d);
    Serial.println(buf);

    while(Serial.read()!='n');
  }

}
#endif //DEBUG