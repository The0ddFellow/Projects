#include <Servo.h>
#include <SevSeg.h>

Servo speedServo;                 // create servo object to control a servo
SevSeg sevseg; 

const int servoMax = 35;          // speed where servo should be at 180 degrees
const int servoMin = 0;           // speed where servo should be at 0 degrees
const int hallPin = 2;            // initializing a pin for the sensor output
const int ledPin =  13;           // initializing a pin for the led. Arduino has built in led attached to pin 13

const int red_light_pin= 11;      // pins for LED
const int green_light_pin = 10;
const int blue_light_pin = 9;

const float Pi = 3.14159;

float inchesToMeters(float x){
  float result;
  result = x / 39.37;
  return result;
}

float r = inchesToMeters(13);       // radius of wheel in meters (change to where you put the magnet)
const float circ = 2 * Pi * r;      // circumference of the wheel

int hallState = 0;                  // initializing a variable for storing the status of the hall sensor.
int servoPos = 180;                 // variable to store the servo position
float time_new = 0;
float time_old = 0;
float period = 0;                   // time it takes for one wheel rotation
float currSpeed = 0;                // current speed in meters per second
float currSpeedkmh = 0;             // current speed in kilometers per hour
int rot = 0;                        // counts the number of rotations
float dist = 0 ;                    // distance traveled in 100 meters
bool j = false;                     // making sure each time count happens only once every rotation

void setup() {
  speedServo.attach(6);             // attaches the servo on pin 6 to the servo object
  speedServo.write(servoPos);       // start the motor at location 0

  pinMode(red_light_pin, OUTPUT);
  pinMode(green_light_pin, OUTPUT);
  pinMode(blue_light_pin, OUTPUT);
  
  byte numDigits = 1;
  byte digitPins[] = {};
  byte segmentPins[] = {8, 7, 3, 4, 5, 12, 13};
  bool resistorsOnSegments = true;
  byte hardwareConfig = COMMON_CATHODE; 
  
  sevseg.begin(hardwareConfig, numDigits, digitPins, segmentPins, resistorsOnSegments);
  sevseg.setBrightness(90);
  
  pinMode(ledPin, OUTPUT);        // This will initialize the LED pin as an output pin
  pinMode(hallPin, INPUT);        // This will initialize the hall effect sensor pin as an input pin to the Arduino

  RGB_color(0, 255, 0);           // starting the LED with a green
  
  Serial.begin(9600);
  while(!Serial);
  Serial.println("Starting...");
}

void loop() {

  hallState = digitalRead(hallPin);                     // reading from the sensor and storing the state of the hall effect sensor :

  if (hallState == LOW) {                               // Checking whether the state of the module is high or low
    if (!j) {
      time_new = millis();
      if (time_old != 0) {                    
        period = (time_new - time_old) / 1000;          // getting the time between sensor readings
        currSpeed = circ / period;                      // getting the speed in meters per second
        currSpeedkmh = (currSpeed * 3600) / 1000;
        dist = (rot * circ) / 100;                     // getting distance traveled in 100 meters
        Serial.println((String)"Speed: " + currSpeed);
        Serial.println((String)"Speed in km: " + currSpeedkmh);
        Serial.println((String)"Distance: " + dist);
        sevseg.setNumber(dist);
        sevseg.refreshDisplay(); 
      } else {
        sevseg.setNumber(0);
        sevseg.refreshDisplay(); 
      }
      time_old = time_new;
      rot ++;
    }
    j = true;
  }

  else {
    if (currSpeedkmh <= 25.0) {           // changing the color of the LED depending on the speed
      RGB_color(0, 255, 0);               // Green
    } else if (currSpeedkmh > 25.0 && currSpeedkmh < 30.0) {
      RGB_color(255, 255, 0);             // Yellow
    } else {
      RGB_color(255, 0, 0);               // Red
    }
    j = false;
  }

  if (currSpeed <= servoMax) {
    servoPos = (currSpeed / servoMax) * 180;
    speedServo.write(180 - servoPos);
  } else {
    speedServo.write(0);
  }
}

void RGB_color(int red_light_value, int green_light_value, int blue_light_value)
 {
  analogWrite(red_light_pin, red_light_value);
  analogWrite(green_light_pin, green_light_value);
  analogWrite(blue_light_pin, blue_light_value);
}
