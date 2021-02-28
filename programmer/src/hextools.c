/* Some handy Hex manipulation functions
 * Source:[TommyPROM/TommyPROM.ino at master · TomNisbet/TommyPROM](https://github.com/TomNisbet/TommyPROM/blob/master/TommyPROM/TommyPROM.ino)
 */

#include <Arduino.h>

const char hex[] = "0123456789abcdef";

/************************************************************
* convert a single hex character [0-9a-fA-F] to its value
* @param char c single character (digit)
* @return byte value of the digit (0-15)
************************************************************/
byte hexDigit(char c)
{
    if ((c >= '0') && (c <= '9'))
    {
        return c - '0';
    }
    else if ((c >= 'a') && (c <= 'f'))
    {
        return c - 'a' + 10;
    }
    else if ((c >= 'A') && (c <= 'F'))
    {
        return c - 'A' + 10;
    }
    else
    {
        return 0xff;
    }
}

/************************************************************
* Convert a hex string to a uint32_t value.
* Skips leading spaces and terminates on the first non-hex
* character.  Leading zeroes are not required.
*
* No error checking is performed - if no hex is found then
* defaultValue is returned.  Similarly, a hex string of more than
* 8 digits will return the value of the last 8 digits.
* @param pointer to string with the hex value of the word (modified)
* @return unsigned int represented by the digits
************************************************************/
uint32_t getHex32(char * pData, uint32_t defaultValue)
{
    uint32_t u32 = 0;

    while (isspace(*pData))
    {
        ++pData;
    }

    if (isxdigit(*pData))
    {
        while (isxdigit(*pData)) {
            u32 = (u32 << 4) | hexDigit(*pData++);
        }
    }
    else
    {
        u32 = defaultValue;
    }

    return u32;
}
