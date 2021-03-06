// **********************************************************************************************************
// MightyHat gateway base unit sketch that works with MightyHat equipped with RFM69W/RFM69HW/RFM69CW/RFM69HCW
// This will relay all RF data over serial to the host computer (RaspberryPi) and vice versa.
// It will buffer the serial data to ensure host serial requests are not missed.
// http://LowPowerLab.com/MightyHat
// PiGateway project: http://LowPowerLab.com/gateway
// **********************************************************************************
// Copyright Felix Rusu 2020, http://www.LowPowerLab.com/contact
// **********************************************************************************
#define MHAT_VERSION     3  //latest is R4, only change to "2" if you have a MightyHat R2
// ****************************************************************************************
#include <RFM69.h>       //get it here: https://github.com/lowpowerlab/rfm69
#include <RFM69_ATC.h>   //get it here: https://github.com/lowpowerlab/RFM69
#include <RFM69_OTA.h>   //get it here: https://github.com/lowpowerlab/RFM69
#include <SPIFlash.h>    //get it here: https://github.com/lowpowerlab/spiflash
#include <PString.h>     //easy string manipulator: http://arduiniana.org/libraries/pstring/
#include <Streaming.h>   //easy C++ style output operators: http://arduiniana.org/libraries/streaming/
#include "U8glib.h"      //https://bintray.com/olikraus/u8glib/Arduino
                         //u8g compared to adafruit lib: https://www.youtube.com/watch?v=lkWZuAnHa2Y
                         //drawing bitmaps: https://www.coconauts.net/blog/2015/01/19/easy-draw-bitmaps-arduino/
//*****************************************************************************************************************************
// ADJUST THE SETTINGS BELOW DEPENDING ON YOUR HARDWARE/SCENARIO !
//*****************************************************************************************************************************
#define NODEID          1  //the gateway has ID=1
#define NETWORKID     200  //all nodes on the same network can talk to each other
#define FREQUENCY     RF69_915MHZ //Match this with the version of your Moteino! (others: RF69_433MHZ, RF69_868MHZ)
//#define FREQUENCY_EXACT 916000000 //uncomment and set to a specific frequency in Hz, if commented the center frequency is used
#define ENCRYPTKEY    "sampleEncryptKey" //has to be same 16 characters/bytes on all nodes, not more not less!
#define IS_RFM69HW_HCW  //required for RFM69HW/HCW, comment out for RFM69W/CW!
#define ENABLE_ATC    //comment out this line to disable AUTO TRANSMISSION CONTROL //more here: http://lowpowerlab.com/blog/2015/11/11/rfm69_atc-automatic-transmission-control/
#define ENABLE_WIRELESS_PROGRAMMING    //comment out this line to disable Wireless Programming of this gateway node
//#define ENABLE_LCD    //comment this out if you don't have or don't want to use the LCD
//*****************************************************************************************************************************
#define SERIAL_BAUD   115200 //change to 19200 if ENABLE_LCD is left uncommented
#define DEBUG_EN      //comment out if you don't want any serial verbose output (keep out in real use)

#define BTN_LED_RED     9
#define BTN_LED_GRN     6  // This will indicate when Pi has power
#define POWER_LED_RED()    { digitalWrite(BTN_LED_RED, HIGH); digitalWrite(BTN_LED_GRN, LOW); }
#define POWER_LED_GRN()    { digitalWrite(BTN_LED_RED, LOW);  digitalWrite(BTN_LED_GRN, HIGH); }
#define POWER_LED_ORANGE() { digitalWrite(BTN_LED_RED, HIGH); digitalWrite(BTN_LED_GRN, HIGH); }
#define POWER_LED_OFF()    { digitalWrite(BTN_LED_RED, LOW);  digitalWrite(BTN_LED_GRN, LOW); }
#define ON              1
#define OFF             0

#define BUZZER              5     // Buzzer attached to D5 (PWM pin required for tones)
#define BUTTON             A2     // Power button pin
#define BUTTON1            A4     // Backlight control button
#define BUTTON2            A5     // Backlight control button
#define LATCH_EN            4
#define LATCH_VAL           7
#define SIG_SHUTOFF        A3     // Signal to Pi to ask for a shutdown
#define SIG_BOOTOK         A6     // Signal from Pi that it's OK to cutoff power
                                  // !!NOTE!! Originally this was D7 but it was moved to A0 at least temporarily.
                                  // On MightyBoost R1 you need to connect D7 and A0 with a jumper wire.
                                  // The explanation for this is given here: http://lowpowerlab.com/mightyboost/#source
#define BATTERYSENSE       A7     // Sense VBAT_COND signal (when powered externally should read ~3.25v/3.3v (1000-1023), when external power is cutoff it should start reading around 2.85v/3.3v * 1023 ~= 880 (ratio given by 10k+4.7K divider from VBAT_COND = 1.47 multiplier)
                                  // hence the actual input voltage = analogRead(A7) * 0.00322 (3.3v/1024) * 1.47 (10k+4.7k voltage divider ratio)
                                  // when plugged in this should be 4.80v, nothing to worry about
                                  // when on battery power this should decrease from 4.15v (fully charged Lipoly) to 3.3v (discharged Lipoly)
                                  // trigger a shutdown to the target device once voltage is around 3.4v to allow 30sec safe shutdown

#define BATTERY_VOLTS(analog_reading) analog_reading * 0.00322 * 1.51 // 100/66 is the inverse ratio of the voltage divider ( Batt > 1MEG > A7 > 2MEG > GND )
#define LOWBATTERYTHRESHOLD   3.5  // a shutdown will be triggered to the target device when battery voltage drops below this (Volts)
#define CHARGINGTHRESHOLD     4.3
#define RESETHOLDTIME         500 // Button must be hold this many mseconds before a reset is issued (should be much less than SHUTDOWNHOLDTIME)
#define SHUTDOWNHOLDTIME     2000 // Button must be hold this many mseconds before a shutdown sequence is started (should be much less than ForcedShutoffDelay)
#define ShutoffTriggerDelay  6000 // will start checking the SIG_BOOTOK line after this long
#define RESETPULSETIME        500 // When reset is issued, the SHUTOFF signal is held HIGH this many ms
#define ForcedShutoffDelay   7500 // when SIG_BOOTOK==0 (PI in unknown state): if button is held
                                  // for this long, force shutdown (this should be less than RecycleTime)
#define ShutdownFinalDelay   4500 // after shutdown signal is received, delay for this long
                                  // to allow all PI LEDs to stop activity (pulse LED faster)
#define RecycleTime         60000 // window of time in which SIG_BOOTOK is expected to go HIGH
                                  // should be at least 3000 more than Min
                                  // if nothing happens after this window, if button is 
                                  // still pressed, force cutoff power, otherwise switch back to normal ON state
#define BATTERYREADINTERVAL   2000

#ifdef DEBUG_EN
  #define DEBUG(input)   Serial.print(input)
  #define DEBUGln(input) Serial.println(input)
#else
  #define DEBUG(input)
  #define DEBUGln(input)
#endif

#define PRINT_UPTIME Serial<< F("UPTIME:") << millis() << endl;
#define PRINT_FREQUENCY Serial << F("SYSFREQ:") << radio.getFrequency() << endl;

#define LED_HIGH digitalWrite(LED_BUILTIN, HIGH)
#define LED_LOW digitalWrite(LED_BUILTIN, LOW)

//******************************************** BEGIN ADVANCED variables ********************************************************************************
#define RAMSIZE 2048
#define MAX_BUFFER_LENGTH       61 //limit parameter update requests to 40 chars. ex: Parameter:LongRequest
#define MAX_ACK_REQUEST_LENGTH  30 //60 is max for ACK (with ATC enabled), but need to allow appending :OK and :INV to confirmations from node

typedef struct req {
  uint16_t nodeId;
  char data[MAX_BUFFER_LENGTH]; //+1 for the null terminator
  struct req *next;
}REQUEST;

//dynamically allocated queue (FIFO) data structure
REQUEST* queue = NULL;
byte size_of_queue = 0;
//******************************************** END ADVANCED variables ********************************************************************************
//******************************************** BEGIN FUNCTION prototypes ********************************************************************************
boolean BOOTOK();
void POWER(uint8_t ON_OFF);
void Beep(byte theDelay, boolean twoSounds);
int freeRAM();
void handleSerialData();
void printQueue(REQUEST* p);
//******************************************** END FUNCTION prototypes ********************************************************************************
//******************************************** BEGIN GENERAL variables ********************************************************************************
byte lastValidReading = 1;
unsigned long lastValidReadingTime = 0;
unsigned long NOW=0;
byte PowerState = OFF;
long lastPeriod = -1;
int rssi=0;
float systemVoltage = 5;
float systemVoltagePrevious = 5;
boolean batteryLow=false;
boolean batteryLowShutdown=false;

SPIFlash flash(SS_FLASHMEM, 0xEF30); //EF30 for 4mbit Windbond FLASH MEM 
#ifdef ENABLE_ATC
  RFM69_ATC radio;
#else
  RFM69 radio;
#endif
//******************************************** END GENERAL variables ********************************************************************************
//******************************************** BEGIN LCD STUFF ********************************************************************************
char buff[80];
PString Pbuff(buff, sizeof(buff)); //easy string manipulator

#ifdef ENABLE_LCD
#if defined(MHAT_VERSION) && (MHAT_VERSION >= 3)
  #define PIN_LCD_CS    A1 //Pin 2 on LCD, lcd CS is shared with Latch value pin since they are both outputs and never HIGH at the same time
  #define PIN_LCD_RST   U8G_PIN_NONE //this is tied directly to the atmega RST
#else
  #define PIN_LCD_CS    LATCH_VAL //Pin 2 on LCD, lcd CS is shared with Latch value pin since they are both outputs and never HIGH at the same time
  #define PIN_LCD_RST   A1 //Pin 1 on LCD
#endif

#define PIN_LCD_DC    A0 //Pin 3 on LCD
#define PIN_LCD_LIGHT 3 //Backlight pin
#define xbmp_logo_width 30
#define xbmp_logo_height 27
#define BACKLIGHTLEVELS  5 //5 levels gives a nice round number that allows full brightness
void LCD_BACKLIGHT(byte level) { if (level>BACKLIGHTLEVELS) level=BACKLIGHTLEVELS; analogWrite(PIN_LCD_LIGHT, 255-level*255/BACKLIGHTLEVELS); }
byte backlightLevel=BACKLIGHTLEVELS; //max at startup

const uint8_t xbmp_logo[] PROGMEM = {
   0xe0, 0xff, 0xff, 0x01, 0xf0, 0xff, 0xff, 0x03, 0x08, 0x00, 0x00, 0x04,
   0x06, 0x00, 0x00, 0x18, 0xc3, 0x03, 0xf0, 0x30, 0x23, 0x04, 0x08, 0x31,
   0x23, 0x04, 0x08, 0x31, 0x23, 0x0c, 0x0c, 0x31, 0xc3, 0x13, 0xf2, 0x30,
   0x03, 0xe0, 0x01, 0x30, 0x03, 0xe0, 0x01, 0x30, 0xc3, 0xe3, 0xf1, 0x30,
   0x23, 0xe4, 0x09, 0x31, 0x23, 0xfc, 0x0f, 0x31, 0x23, 0xe4, 0x09, 0x31,
   0xc3, 0xe3, 0xf1, 0x30, 0x03, 0xe0, 0x01, 0x30, 0x03, 0xe0, 0x01, 0x30,
   0xc3, 0x13, 0xf2, 0x30, 0x23, 0x0c, 0x0c, 0x31, 0x23, 0x04, 0x08, 0x31,
   0x23, 0x04, 0x08, 0x31, 0xc3, 0x03, 0xf0, 0x30, 0x06, 0x00, 0x00, 0x18,
   0x08, 0x00, 0x00, 0x04, 0xf0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x01 };
   
#define xbmp_batt_width 9
#define xbmp_batt_height 6
const uint8_t xbmp_batt_c[] PROGMEM = { 0xff, 0x00, 0xbf, 0x00, 0x9f, 0x01, 0x8f, 0x01, 0x87, 0x00, 0xff, 0x00 };
const uint8_t xbmp_batt_x[] PROGMEM = { 0xff, 0x00, 0xa5, 0x00, 0x81, 0x01, 0x99, 0x01, 0xa5, 0x00, 0xff, 0x00 };

const uint8_t xbmp_batt_0[] PROGMEM = {  };
const uint8_t xbmp_batt_1[] PROGMEM = { 0xff, 0x00, 0x83, 0x00, 0x83, 0x01, 0x83, 0x01, 0x83, 0x00, 0xff, 0x00 };
const uint8_t xbmp_batt_2[] PROGMEM = { 0xff, 0x00, 0x87, 0x00, 0x87, 0x01, 0x87, 0x01, 0x87, 0x00, 0xff, 0x00 };
const uint8_t xbmp_batt_3[] PROGMEM = { 0xff, 0x00, 0x8f, 0x00, 0x8f, 0x01, 0x8f, 0x01, 0x8f, 0x00, 0xff, 0x00 };
const uint8_t xbmp_batt_4[] PROGMEM = { 0xff, 0x00, 0x9f, 0x00, 0x9f, 0x01, 0x9f, 0x01, 0x9f, 0x00, 0xff, 0x00 };
const uint8_t xbmp_batt_5[] PROGMEM = { 0xff, 0x00, 0xbf, 0x00, 0xbf, 0x01, 0xbf, 0x01, 0xbf, 0x00, 0xff, 0x00 };
const uint8_t xbmp_batt_6[] PROGMEM = { 0xff, 0x00, 0xff, 0x00, 0xff, 0x01, 0xff, 0x01, 0xff, 0x00, 0xff, 0x00 };

#define xbmp_rssi_width 7
#define xbmp_rssi_height 6
const uint8_t xbmp_rssi_1[] PROGMEM = { 0x40, 0x10, 0x00, 0x04, 0x04, 0x05 };
const uint8_t xbmp_rssi_2[] PROGMEM = { 0x40, 0x10, 0x10, 0x14, 0x14, 0x15 };
const uint8_t xbmp_rssi_3[] PROGMEM = { 0x40, 0x50, 0x50, 0x54, 0x54, 0x55 };
const uint8_t xbmp_rssi_0[] PROGMEM = { 0x40, 0x10, 0x00, 0x04, 0x00, 0x01 };

U8GLIB_PCD8544 lcd(PIN_LCD_CS, PIN_LCD_DC, PIN_LCD_RST); //hardware SPI
//U8GLIB_PCD8544 lcd(SCK, MOSI, PIN_LCD_CS, PIN_LCD_DC , PIN_LCD_RST); //software SPI

//******************************************** LCD FUNCTIONS ********************************************************************************
void drawLogo() {
  lcd.firstPage();
  do {
    lcd.drawXBMP((84-xbmp_logo_width)/2, (48-xbmp_logo_height)/2, xbmp_logo_width, xbmp_logo_height, xbmp_logo); //tutorial: https://www.coconauts.net/blog/2015/01/19/easy-draw-bitmaps-arduino/
  } while(lcd.nextPage());
}

void clearDisplay() { lcd.firstPage(); do{}while(lcd.nextPage()); }

//******************************************** MESSAGE HISTORY ******************************************************************************
#define MSG_MAX_LEN   32    //truncate message at 32 chars since most are shorter than that anyway
#define HISTORY_LEN   10    //hold this many past messages (IMPORTANT: 10 records needs about 330 bytes of RAM so be careful about making this too large)
typedef struct {
  char data[MSG_MAX_LEN];
  int rssi;
} Message;
Message * messageHistory = new Message[HISTORY_LEN];
byte lastMessageIndex = HISTORY_LEN;
byte currMessageIndex = HISTORY_LEN;
byte historyLength = 0;

void saveToHistory(char * msg, int rssi)
{
  byte length = strlen(msg);
  byte i = 0;
  if (lastMessageIndex >= HISTORY_LEN-1) lastMessageIndex = 0;
  else lastMessageIndex++;
  if (historyLength < HISTORY_LEN) historyLength++;
  currMessageIndex = historyLength-1; //reset history pointer back to latest message

  for (; i<(MSG_MAX_LEN-1) && (i < length); i++)
    messageHistory[lastMessageIndex].data[i] = msg[i];

  messageHistory[lastMessageIndex].data[i] = '\0'; //terminate string
  messageHistory[lastMessageIndex].rssi = rssi;
}
//******************************************** END MESSAGE HISTORY **************************************************************************

void refreshLCD() {
  noInterrupts(); //while messing with LCD need to pause interrups from radio to avoid SPI conflicts!
  byte lcdwidth = lcd.getWidth();
  byte lcdheight = lcd.getHeight();
  char c;
  byte i,pos,swidth;
  byte * bmpPtr;
      
  //u8glib picture loop
  lcd.firstPage();
  do {
    lcd.setFont(u8g_font_profont10);
    lcd.setFontRefHeightText();
    lcd.setFontPosTop();
    byte fontheight = lcd.getFontAscent()-lcd.getFontDescent();

    char * textp = buff;
    if (historyLength > 0)
      textp = messageHistory[currMessageIndex].data;

    byte textLength = strlen(textp);
    byte line=0;
    byte done = false;

    //this section splits the textp string into chunks that fit on the screen width and prints each to a new line
    while(textLength && !done)
    {
      for (i=1;i<=textLength;i++)
      {
        c = textp[i];
        textp[i]=0;
        swidth = lcd.getStrWidth(textp);
        textp[i] = c;
        if (c=='\n') { pos = i; break; } //newline char found, skip it and go to next line
        if (swidth > lcdwidth) { pos = i-1; break; } //line is full, go to next line
        else if (i==textLength) { done = true; }
      }
      if (!done)
      {
        c = textp[pos];
        textp[pos]=0;
      }
      lcd.drawStr(0, line * fontheight, textp);
      if (done) break;
      textp[pos] = c;
      textp += pos;
      textLength -= pos;
      line++;
    }

    lcd.setFontPosBaseline();

    //print battery voltage and icon
    if (systemVoltage >= 4.3) bmpPtr = (byte*)xbmp_batt_c;
    else if (systemVoltage >= 4) bmpPtr = (byte*)xbmp_batt_6;
    else if (systemVoltage >= 3.9) bmpPtr = (byte*)xbmp_batt_5;
    else if (systemVoltage >= 3.8) bmpPtr = (byte*)xbmp_batt_4;
    else if (systemVoltage >= 3.7) bmpPtr = (byte*)xbmp_batt_3;
    else if (systemVoltage >= 3.6) bmpPtr = (byte*)xbmp_batt_2;
    else if (systemVoltage >= 3.5) bmpPtr = (byte*)xbmp_batt_1;
    else bmpPtr = (byte*)xbmp_batt_x;
    lcd.drawXBMP(lcdwidth-xbmp_batt_width, lcdheight-xbmp_batt_height, xbmp_batt_width, xbmp_batt_height, bmpPtr);

    lcd.setPrintPos(54, 48);
    if (systemVoltage >= CHARGINGTHRESHOLD)
      lcd.print("CHRG"); 
    else
      lcd.print(systemVoltage);
  
    lcd.setPrintPos(0, 40);
    uint16_t uptimeSeconds = millis()/1000;
    Pbuff="";
    if (uptimeSeconds<60)
      Pbuff << "up" << uptimeSeconds << 's';
    else
      Pbuff << "up:" << (uptimeSeconds/60) << 'm';
    lcd.print(buff);

    lcd.setPrintPos(45, 40);
    Pbuff="";
    Pbuff << "RAM:" << freeRAM();
    lcd.print(buff);

    //print rssi and icon
    if (rssi > -70) bmpPtr = (byte*)xbmp_rssi_3;
    else if (rssi > -80) bmpPtr = (byte*)xbmp_rssi_2;
    else if (rssi > -90) bmpPtr = (byte*)xbmp_rssi_1;
    else if (rssi > -95) bmpPtr = (byte*)xbmp_rssi_0;
    lcd.drawXBMP(0, lcdheight-xbmp_rssi_height, xbmp_rssi_width, xbmp_rssi_height, bmpPtr);
    if (rssi !=0) {
      Pbuff="";
      Pbuff << rssi << "dBm";
      lcd.drawStr(xbmp_rssi_width+1, 48, buff);
    }
  } while(lcd.nextPage());
  digitalWrite(PIN_LCD_CS, HIGH);
  interrupts(); //re-enable interrupts
}
#endif //ENABLE_LCD
//******************************************** END LCD STUFF ********************************************************************************

void setupPowerControl(){
  pinMode(BUTTON, INPUT_PULLUP);
  pinMode(SIG_BOOTOK, INPUT);
  pinMode(SIG_SHUTOFF, OUTPUT);
  pinMode(BTN_LED_RED, OUTPUT);
  pinMode(BTN_LED_GRN, OUTPUT);
  pinMode(LATCH_EN, OUTPUT);
  digitalWrite(LATCH_EN, LOW);
#ifdef ENABLE_LCD
  pinMode(PIN_LCD_CS, OUTPUT);
  digitalWrite(PIN_LCD_CS, HIGH);
#endif
  pinMode(LATCH_VAL, OUTPUT);
  pinMode(BUTTON1, INPUT_PULLUP);
  pinMode(BUTTON2, INPUT_PULLUP);
  pinMode(BATTERYSENSE, INPUT);
  digitalWrite(SIG_SHUTOFF, LOW);//added after sudden shutdown quirks, DO NOT REMOVE!
}

void handlePowerControl() {
  byte reading = digitalRead(BUTTON);
  NOW = millis();
  digitalWrite(SIG_SHUTOFF, LOW);//added after sudden shutdown quirks, DO NOT REMOVE!
  
  //artificial power ON after a low battery shutdown
  if (PowerState == OFF && batteryLowShutdown && systemVoltage >= CHARGINGTHRESHOLD)
    reading = HIGH;
  
  if ((PowerState == ON && batteryLow) || (reading != lastValidReading && NOW - lastValidReadingTime > 200))
  {
    lastValidReading = reading;
    lastValidReadingTime = NOW;
    
    if ((PowerState == ON && batteryLow) || reading == LOW)
    {
      radio.sleep();
      //make sure the button is held down for at least 'RESETHOLDTIME' before taking action (this is to avoid accidental button presses and consequently Pi shutdowns)
      NOW = millis();
      while (!batteryLow && (PowerState == ON && millis()-NOW < RESETHOLDTIME)) { delay(10); if (digitalRead(BUTTON) != 0) return; }

      //RESETHOLDTIME is satisfied, now check if button still held until SHUTDOWNHOLDTIME is satisfied
      POWER_LED_ORANGE(); //make the button LED orange to show something's going on
      while (!batteryLow && (PowerState == ON && millis()-NOW < SHUTDOWNHOLDTIME))
      {
        if (digitalRead(BUTTON) != 0)
        {
          if (BOOTOK())       //SIG_BOOTOK is HIGH so Pi is running the shutdowncheck.sh script, ready to intercept the RESET PULSE
          {
#ifdef ENABLE_LCD
            Pbuff="";
            Pbuff << "Rebooting Pi..";
            saveToHistory(buff, 0);
            refreshLCD();
#endif
            digitalWrite(SIG_SHUTOFF, HIGH);
            delay(RESETPULSETIME);
            digitalWrite(SIG_SHUTOFF, LOW);

            NOW = millis();
            boolean recycleDetected=false;
            while (millis()-NOW < RecycleTime) //blink LED while waiting for BOOTOK to go high
            {
              //blink 3 times and pause
              POWER_LED_OFF(); //digitalWrite(POWER_LED, LOW);
              delay(100);
              POWER_LED_ORANGE(); //digitalWrite(POWER_LED, HIGH);
              delay(100);
              POWER_LED_OFF(); //digitalWrite(POWER_LED, LOW);
              delay(100);
              POWER_LED_ORANGE(); //digitalWrite(POWER_LED, HIGH);
              delay(100);
              POWER_LED_OFF(); //digitalWrite(POWER_LED, LOW);
              delay(100);
              POWER_LED_ORANGE(); //digitalWrite(POWER_LED, HIGH);
              delay(500);

              if (!BOOTOK()) recycleDetected = true;
              else if (BOOTOK() && recycleDetected)
              {
#ifdef ENABLE_LCD
                Pbuff="";
                Pbuff << "Reboot OK!";
                saveToHistory(buff, 0);
                refreshLCD();
#endif
                return;
              }
            }
            return; //reboot pulse sent but it appears a reboot failed; exit all checks
          }
          else return; //ignore everything else (button was held for RESETHOLDTIME, but SIG_BOOTOK was LOW)
        }
      }
      
      //SIG_BOOTOK must be HIGH when Pi is ON. During boot, this will take a while to happen (till it executes the "shutdowncheck" script)
      //so I dont want to cutoff power before it had a chance to fully boot up
      if ((batteryLow || PowerState == ON) && BOOTOK())
      {
        if (batteryLow) {
#ifdef ENABLE_LCD
          Pbuff="";
          Pbuff << "Battery low! Shutting down Pi..";
          saveToHistory(buff, 0);
#endif
          batteryLowShutdown = true;
        }
#ifdef ENABLE_LCD
        else {
          Pbuff="";
          Pbuff << "Shutting down Pi..";
          saveToHistory(buff, 0);
        }
        refreshLCD();
#endif

        // signal Pi to shutdown
        digitalWrite(SIG_SHUTOFF, HIGH);

        //now wait for the Pi to signal back
        NOW = millis();
        float in, out;
        boolean forceShutdown = true;

        POWER_LED_OFF();
        while (millis()-NOW < RecycleTime)
        {
          if (in > 6.283) in = 0;
          in += .00628;

          out = sin(in) * 127.5 + 127.5;
          analogWrite(BTN_LED_RED, out);
          delayMicroseconds(1500);
          
          //account for force-shutdown action (if button held for ForcedShutoffDelay, then force shutdown regardless)
          if (millis()-NOW <= (ForcedShutoffDelay-SHUTDOWNHOLDTIME) && digitalRead(BUTTON) != 0)
            forceShutdown = false;
          if (millis()-NOW >= (ForcedShutoffDelay-SHUTDOWNHOLDTIME) && forceShutdown)
          {
            PowerState = OFF;
            POWER_LED_OFF(); //digitalWrite(POWER_LED, PowerState); //turn off LED to indicate power is being cutoff
            POWER(PowerState);
            break;
          }

          if (millis() - NOW > ShutoffTriggerDelay)
          {
            // Pi signaling OK to turn off
            if (!BOOTOK())
            {
              PowerState = OFF;
              POWER_LED_OFF(); //digitalWrite(POWER_LED, PowerState); //turn off LED to indicate power is being cutoff
              NOW = millis();
              while (millis()-NOW < ShutdownFinalDelay)
              {
                if (in > 6.283) in = 0;
                in += .00628;
                out = sin(in) * 127.5 + 127.5;
                analogWrite(BTN_LED_RED,out);
                delayMicroseconds(300);
              }

              POWER(PowerState);
              break;
            }
          }
        }

        // last chance: if power still on but button still pressed, force cutoff power
        if (PowerState == ON && digitalRead(BUTTON) == 0)
        {
          PowerState = OFF;
          POWER(PowerState);
        }

#ifdef ENABLE_LCD
        if (PowerState == OFF)
        {
          Pbuff="";
          Pbuff << "Pi is now OFF";
          saveToHistory(buff, 0);
          refreshLCD();
        }
#endif

        digitalWrite(SIG_SHUTOFF, LOW);
      }
      else if (PowerState == ON && !BOOTOK())
      {
#ifdef ENABLE_LCD
        Pbuff="";
        Pbuff << "Forced shutdown..";
        saveToHistory(buff, 0);
        refreshLCD();
#endif
        NOW = millis();
        unsigned long NOW2 = millis();
        int analogstep = 255 / ((ForcedShutoffDelay-SHUTDOWNHOLDTIME)/100); //every 500ms decrease LED intensity
        while (digitalRead(BUTTON) == 0)
        {
          if (millis()-NOW2 > 100)
          {
            analogWrite(BTN_LED_RED, 255 - ((millis()-NOW)/100)*analogstep);
            NOW2 = millis();
          }
          if (millis()-NOW > ForcedShutoffDelay-SHUTDOWNHOLDTIME)
          {
            //TODO: add blinking here to signal final shutdown delay
            PowerState = OFF;
            POWER(PowerState);
#ifdef ENABLE_LCD
            Pbuff="";
            Pbuff << "Pi is now OFF";
            saveToHistory(buff, 0);
            refreshLCD();
#endif
            break;
          }
        }
      }
      else if (PowerState == OFF)
      {
        PowerState = ON;
        batteryLowShutdown=false;
        POWER(PowerState);
#ifdef ENABLE_LCD
        Pbuff="";
        Pbuff << "Pi is now ON";
        saveToHistory(buff, 0);
        refreshLCD();
#endif
      }
    }

    if (PowerState == ON) POWER_LED_GRN() else POWER_LED_OFF(); //digitalWrite(POWER_LED, PowerState);
  }
}

uint32_t buttonsLastChanged;
void handle2Buttons()
{
  if (millis() - buttonsLastChanged < 200) return; //basic button debouncing & prevent changing level too fast

  //button 1 - backlight
  if (digitalRead(BUTTON1)==LOW)
  {
    buttonsLastChanged=millis();
    Beep(3, false);
#ifdef ENABLE_LCD
    if (backlightLevel==BACKLIGHTLEVELS) backlightLevel=0;
    else backlightLevel++;
    LCD_BACKLIGHT(backlightLevel);
#endif
  }
  
  //button 2 - message history
  if (digitalRead(BUTTON2)==LOW)
  {
    buttonsLastChanged=millis();
    Beep(3, false);

#ifdef ENABLE_LCD
    if (historyLength > 0) //if at least 1 data packet was received and saved to history...
    {
      rssi = messageHistory[currMessageIndex].rssi;                     //save the history rssi for the LCDRefresh signal icon
#ifdef ENABLE_LCD
      Pbuff="";
      Pbuff << "<HIST[" << currMessageIndex+1 << '/' << historyLength << "]>" << endl << messageHistory[currMessageIndex].data;
      refreshLCD(); //paint the screen
#endif
      if (currMessageIndex==0) currMessageIndex=historyLength-1; else currMessageIndex--; //this makes it cycle from the latest message towards oldest as you press BTN2
    }
#endif
  }
}

boolean BOOTOK() {
  return analogRead(SIG_BOOTOK) > 800; //the BOOTOK signal is on an analog pin because a digital may not always pick it up (its less than 3.3v)
}

void POWER(uint8_t ON_OFF) {
  digitalWrite(LATCH_EN, HIGH);
  digitalWrite(LATCH_VAL, ON_OFF);
  delay(5);
  digitalWrite(LATCH_EN, LOW);
  delay(5);
#ifdef ENABLE_LCD
  digitalWrite(PIN_LCD_CS, HIGH); //if shared with LATCH_VAL, should be HIGH when not used by latch
#endif
}

void Beep(byte theDelay, boolean twoSounds)
{
  if (theDelay > 20) theDelay = 20;
  tone(BUZZER, 4200); //4200
  delay(theDelay);
  noTone(BUZZER);
  delay(10);
  if (twoSounds)
  {
    tone(BUZZER, 4500); //4500
    delay(theDelay);
    noTone(BUZZER);
  }
}

boolean readBattery() {
  //periodically read the battery voltage
  int currPeriod = millis()/BATTERYREADINTERVAL;
  if (currPeriod != lastPeriod)
  {
    lastPeriod=currPeriod;
    systemVoltage = BATTERY_VOLTS(analogRead(BATTERYSENSE));
    //dtostrf(systemVoltage, 3,2, BATstr);
    batteryLow = systemVoltage < LOWBATTERYTHRESHOLD;
    return true; //signal that batt has been read
  }
  return false;
}

void setup() {
  Beep(20, false);delay(50);Beep(20, false);delay(50);Beep(20, false);
  setupPowerControl();
  Serial.begin(SERIAL_BAUD);
  pinMode(LED_BUILTIN, OUTPUT);
  LED_HIGH;

  radio.initialize(FREQUENCY,NODEID,NETWORKID);
#ifdef ENCRYPTKEY
  radio.encrypt(ENCRYPTKEY);
#endif
#ifdef IS_RFM69HW_HCW
  radio.setHighPower();
#endif
  Serial << endl << "GATEWAYSTART" << endl;
  PRINT_FREQUENCY;
  PRINT_UPTIME;

  if (!flash.initialize()) DEBUGln(F("DEBUG:SPI_Flash_Init_FAIL"));

#ifdef FREQUENCY_EXACT
  radio.setFrequency(FREQUENCY_EXACT); //set frequency to some custom frequency
#endif

  readBattery();
  DEBUG(F("FREERAM:"));DEBUGln(freeRAM());

#ifdef ENABLE_LCD
  pinMode(PIN_LCD_LIGHT, OUTPUT);  //LCD backlight, LOW = backlight ON
  lcd.setRot180();  //rotate screen 180 degrees
  lcd.setContrast(140); //120-160 seems to be usable range
  drawLogo();
  LCD_BACKLIGHT(backlightLevel);
  delay(2000);
  refreshLCD();
  delay(1000);
#endif
  LED_LOW;
}

boolean newPacketReceived;
void loop() {
  handlePowerControl(); //checks any button presses and takes action
  handle2Buttons();     //checks the general purpose buttons next to the LCD (R2+)
  handleSerialData();   //checks for any serial input from the Pi computer

  //process any received radio packets
  if (radio.receiveDone())
  {
    LED_HIGH;
    rssi = radio.RSSI; //get this asap from transceiver
    if (radio.DATALEN > 0) //data packets have a payload
    {
      for (byte i=9;i<radio.DATALEN;i++) {
        if (radio.DATA[i]=='\n' || radio.DATA[i]=='\r')
          radio.DATA[i]=' '; //remove any newlines in the payload - this should only ever happen with noise data that actually made it through
      }
      Pbuff="";
      Pbuff << '[' << radio.SENDERID << "] " << (char*)radio.DATA;
      Serial << buff << F(" SS:") << rssi << endl; //this passes data to MightyHat / RaspberryPi
#ifdef ENABLE_LCD
      saveToHistory(buff, rssi);
#endif
    }

    //check if the packet is a wireless programming request
#ifdef ENABLE_WIRELESS_PROGRAMMING
    CheckForWirelessHEX(radio, flash, false); //non verbose DEBUG
#endif

    //respond to any ACK if requested
    if (radio.ACKRequested())
    {
      REQUEST* aux=queue;
      Pbuff="";
      //walk queue and add pending commands to ACK payload (as many it can fit)
      while (aux!=NULL) {
        if (aux->nodeId==radio.SENDERID)
        {
          //check if payload has room to add this queued command
          if (Pbuff.length() + 1 + strlen(aux->data) <= MAX_ACK_REQUEST_LENGTH)
          {
            if (Pbuff.length()) Pbuff.print(' '); //prefix with a space any previous command in buffer
            Pbuff.print(aux->data); //append command
          }
        }
        aux=aux->next;
      }
      if (Pbuff.length())
        radio.sendACK(buff, Pbuff.length());
      else
        radio.sendACK();
    }
    LED_LOW;
    newPacketReceived = true;
  }

  readBattery();

#ifdef ENABLE_LCD
  if (newPacketReceived || systemVoltagePrevious-systemVoltage > 0.01 || systemVoltagePrevious-systemVoltage < -0.1)
  {
    systemVoltagePrevious = systemVoltage;
    newPacketReceived = false;
    refreshLCD();
  }
  LCD_BACKLIGHT(batteryLow ? 0 : backlightLevel);
#endif
}

boolean insert(uint16_t new_id, char new_data[]) { 
  REQUEST* aux;
  REQUEST* new_node = (REQUEST*)malloc(sizeof(REQUEST));
  if (new_node == NULL) return false;
  new_node->nodeId = new_id; 
  strcpy(new_node->data, new_data);
  new_node->next = NULL;
  if (queue == NULL) queue = new_node;
  else {
      aux = queue;
      while(aux->next != NULL) aux=aux->next;
      aux->next=new_node;
  }
  return true;
}

//processCommand - parse the command and send it to target
//if target is non-responsive it(sleeppy node?) then queue command to send when target wakes and asks for an ACK
//SPECIAL COMMANDS FROM HOST:
// - RQ:123:MESSAGE - send or (upon fail) queue message
// - 123:VOID - removes all queued commands for node 123
// - 123:VOID:command - removes 'command' from queue (if found)
// - RQ - prints the queued list of nodes on serial port, to host (Pi?)
// - RQ:VOID - flush entire queue
// - FREERAM - returns # of unallocated bytes at end of heap
// - SYSFREQ - returns operating frequency in Hz
// - UPTIME - returns millis()
void processCommand(char data[], boolean allowDuplicate=false) {
  char *ptr;
  char dataPart[MAX_BUFFER_LENGTH];
  uint16_t targetId;
  byte sendLen = 0;
  byte isQueueRequest = false;
  ptr = strtok(data, ":");

  if (strcmp(data, "FREERAM")==0)
    Serial << F("FREERAM:") << freeRAM() << ':' << RAMSIZE << endl;
  if (strcmp(data, "RQ")==0)
  {
    ptr = strtok(NULL, ":");  //move to next :
    if (ptr == NULL) printQueue(queue);
    else isQueueRequest = true;
  }
  if (strcmp(data, "SYSFREQ")==0)
    PRINT_FREQUENCY;
  if (strcmp(data, "UPTIME")==0)
    PRINT_UPTIME;
  if (strcmp(data, "NETWORKID")==0)
    Serial << F("NETWORKID:") << NETWORKID << endl;
  if (strcmp(data, "BEEP")==0) Beep(5, false);
  if (strcmp(data, "BEEP2")==0) Beep(10, false);
  if (strcmp(data, "ENCRYPTKEY")==0)
#ifdef ENCRYPTKEY
    Serial << F("ENCRYPTKEY:") << ENCRYPTKEY << endl;
#else
    Serial << F("ENCRYPTKEY:NONE") << endl;
#endif

  if(ptr != NULL) {                  // delimiter found, valid command
    sprintf(dataPart, "%s", ptr);

    //if "RQ:VOID" then flush entire requst queue
    if (isQueueRequest && strcmp(dataPart, "VOID")==0) {
      REQUEST* aux = queue;
      byte removed=0;
  
      while(aux != NULL) {
        if (aux == queue) {
          if (aux->next == NULL) {
            free(queue);
            queue=NULL;
            removed++;
            break;
          }
          else {
            queue = queue->next;
            free(aux);
            removed++;
            aux = queue;
            continue;
          }
        }
      }
      DEBUG("DEBUG:VOIDED_commands:");DEBUGln(removed);
      size_of_queue = size_of_queue - removed;
      return;
    }

    targetId = atoi(dataPart);       // attempt to extract nodeID part
    ptr = strtok(NULL, "");          // get command part to the end of the string
    sprintf(dataPart, "%s", ptr);

    //check for empty command
    if (strlen(dataPart) == 0) return;

    //check target nodeID is valid
    if (targetId > 0 && targetId != NODEID && targetId<=1023) {
      REQUEST* aux;
      byte removed=0;

      //check if VOID command - if YES then remove command(s) to that target nodeID
      if (strstr(dataPart, "VOID")==dataPart) //string starts with VOID
      {
        //if 'nodeId:VOID' then remove all commands to that node
        //if 'nodeId:VOID:REQUEST' then remove just 'REQUEST' (if found & identical match)
        boolean removeAll=true;
        if (dataPart[4]==':' && strlen(dataPart)>5)
          removeAll=false;

        //iterate over queue
        aux = queue;
        while(aux != NULL) {
          if (aux->nodeId==targetId)
          {
            if (removeAll || (!removeAll && strcmp(aux->data, dataPart+5)==0))
            {
              if (aux == queue)
              {
                if (aux->next == NULL)
                {
                  free(queue);
                  queue=NULL;
                  removed++;
                  break;
                }
                else
                {
                  queue = queue->next;
                  free(aux);
                  removed++;
                  aux = queue;
                  continue;
                }
              }
              else
              {
                REQUEST* prev=queue;
                while(prev->next != NULL && prev->next != aux) prev = prev->next; //find previous
                if (prev->next == NULL) break;
                prev->next=prev->next->next;
                free(aux);
                removed++;
                aux=prev->next;
              }
            }
            else aux=aux->next;
          }
          else aux=aux->next;
        }
        DEBUG("DEBUG:VOIDED_commands:");DEBUGln(removed);
        size_of_queue = size_of_queue - removed;
        return;
      }

      //try sending to node, if it fails, continue & add to pending commands queue
      LED_HIGH;
      if (radio.sendWithRetry(targetId, dataPart, strlen(dataPart)))
      {
        LED_LOW;
        return;
      }
      LED_LOW;

      if (!isQueueRequest) return; //just return at this time if not queued request

      //check for duplicate
      if (!allowDuplicate) {
        //walk queue and check for duplicates
        aux = queue;
        while(aux != NULL)
        {
          //DEBUGln("While");
          if (aux->nodeId==targetId)
          {
            if (strcmp(aux->data, dataPart)==0)
            {
              DEBUGln(F("DEBUG:processCommand_skip_duplicate"));  
              return;
            }
          }
          aux = aux->next;
        }
      }

      //all checks OK, attempt to add to queue
      if (insert(targetId, dataPart))
      {
        //DEBUG(F("-> inserted: ")); 
        //DEBUG(targetId);
        //DEBUG("_");
        //DEBUGln(dataPart);
        size_of_queue++;
      }
      else
      {
        DEBUGln(F("DEBUG:INSERT_FAIL:MEM_FULL"));
        Serial << F("[") << targetId << F("] ") << dataPart << F(":MEMFULL") << endl;
      }
    }
    else { 
      //DEBUG(F("DEBUG:INSERT_FAIL - INVALID nodeId:")); DEBUGln(targetId);
      Serial<< '[' << targetId <<"] " << dataPart << F(":INV:ID-OUT-OF-RANGE") << endl;
    }
  }
}

void printQueue(REQUEST* p) {
  if (!size_of_queue) {
    Serial << F("RQ:EMPTY") << endl;
    return;
  }

  REQUEST* aux=p;
  while (aux!=NULL) {
    Serial << F("RQ:") << aux->nodeId << ':' << aux->data << endl;
    aux=aux->next;
  }
}

// here's the processing of single char/bytes as soon as they're coming from UART
void handleSerialData() {
  static char input_line[100]; //static = these get allocated ONCE!
  static byte input_pos = 0;
  if(Serial.available() > 0)
  {
    char inByte = Serial.read();
    switch (inByte)
    {
      case '\r':   //ignore carriage return
        break;

      case '\n':
        if (input_pos==0) break;       // ignore empty lines
        input_line[input_pos] = 0;     // null terminate the string
        DEBUG("DEBUG:handleSerialData:");
        DEBUGln(input_line);
        processCommand(input_line);        // fill up queue
        input_pos = 0; // reset buffer for next time
        break;

      default:
        // keep adding if not full ... allow for terminating byte
        if (input_pos < MAX_BUFFER_LENGTH-1) {
          input_line[input_pos] = inByte;
          input_pos++;
        } else {
          // if theres no EOL coming before MAX_BUFF_CHARS is exceeded we'll just terminate and send it, last char is then lost
          input_line[input_pos] = 0;    // null terminate the string
          DEBUG("DEBUG:MAX_BUFF_CHARS is exceeded - attempting to add (default): ");
          DEBUGln(input_line);
          processCommand(input_line);  //add to queue
          input_pos = 0; //reset buffer for next line
        }
        break;
    }
  }
}

//returns # of unfragmented free RAM bytes (free end of heap)
int freeRAM() {
#ifdef __arm__
  char top;
  return &top - reinterpret_cast<char*>(sbrk(0));
#else
  extern int __heap_start, *__brkval; 
  int v; 
  return (int) &v - (__brkval == 0 ? (int) &__heap_start : (int) __brkval); 
#endif
}

//returns total # of free RAM bytes (all free heap, including fragmented memory)
int allFreeRAM() 
{
  int size = 1024;
  byte *buf;
  while ((buf = (byte *) malloc(--size)) == NULL);
  free(buf);
  return size;
}
