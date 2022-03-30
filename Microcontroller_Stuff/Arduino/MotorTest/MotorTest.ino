#define stp1 2
#define dir1 3
#define M1MS1 4
#define M1MS2 5
#define EN1  6
int x;
int y;
int state;

void setup(){
  // Set up stepper motor
  pinMode(stp1, OUTPUT);
  pinMode(dir1, OUTPUT);
  pinMode(M1MS1, OUTPUT);
  pinMode(M1MS2, OUTPUT);
  pinMode(EN1, OUTPUT);
  resetEDPins1();

  Serial.begin(9600);
}

void loop(){
  digitalWrite(EN1, LOW); //Pull enable pin low to allow motor control  
  digitalWrite(dir1, LOW); //Pull direction pin low to move "forward"
  for(x= 0; x < 1000; x++){  //Loop the forward stepping enough times for motion to be visible
    digitalWrite(stp1, HIGH); //Trigger one step forward
    delay(1);
    digitalWrite(stp1, LOW); //Pull step pin low so it can be triggered again
    delay(1);
  }
  resetEDPins1();
  delay(1000);
}

void resetEDPins1()
{
  digitalWrite(stp1, LOW);
  digitalWrite(dir1, LOW);
  digitalWrite(M1MS1, LOW);
  digitalWrite(M1MS2, LOW);
  digitalWrite(EN1, HIGH);
}
