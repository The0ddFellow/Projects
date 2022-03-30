#include <IRremote.h>
#include <Stepper.h>

#define echoPin 24
#define trigPin 5

const int RECV_PIN = 22;
IRrecv irrecv(RECV_PIN);
decode_results results;
unsigned long key_value = 0;

const int stepsPerRevolution = 2038;
Stepper myStepper = Stepper(stepsPerRevolution, 23, 3, 2, 4);

long duration; 
int distance;

void setup(){
  Serial.begin(9600);
  irrecv.enableIRIn();
  irrecv.blink13(true);

  pinMode(trigPin, OUTPUT);
  pinMode(echoPin, INPUT);
}

void loop(){
  digitalWrite(trigPin, LOW);
  delayMicroseconds(2);
  digitalWrite(trigPin, HIGH);
  delayMicroseconds(10);
  digitalWrite(trigPin, LOW);
  duration = pulseIn(echoPin, HIGH);
  distance = duration * 0.034 / 2; 
  
  if (irrecv.decode(&results)){

    if (results.value == 0XFFFFFFFF){
      results.value = key_value;
    }   
    
    if(results.value == 0xFF906F){             // This one goes up  
      myStepper.setSpeed(700);
      myStepper.step(stepsPerRevolution);
//      if(distance < 20 && distance >= 5){
//        myStepper.setSpeed(100);
//        myStepper.step(stepsPerRevolution);
//      } else {
//        myStepper.setSpeed(500);
//        myStepper.step(stepsPerRevolution);
//      }
    } else if(results.value == 0xFFE01F){      // And this one down
//      if(distance >= 5){
//        if(distance < 15){
//          myStepper.setSpeed(100);
//          myStepper.step(-stepsPerRevolution);
//        } else {
//          myStepper.setSpeed(500);
//          myStepper.step(-stepsPerRevolution);
//        }
//      }
    }
    
    key_value = results.value;
    irrecv.resume();
  }
}
