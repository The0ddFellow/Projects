int ain = A2;            //Analog pin to read signals
int now = 0;            //checks current status of analog pin 2
int prev= 0;            //checks previous state
int count = 0;

unsigned long firstpulse = 0;
unsigned long totaltime = 0;
unsigned long lastpulse = 0;


void setup() {
  // put your setup code here, to run once:
  pinMode(ain,INPUT);
  Serial.begin(115200);
}

void loop() {
  // put your main code here, to run repeatedly:
  now = analogRead(ain);
  delay(1);
  if(now!=prev) 
  {
    if( now >= prev) 
    {
       prev = now;
       Serial.println("Current value is greater than previous value"); 
    }
    
    else
    {
    if(count == 0)
    {
      firstpulse = millis();
      count += 1;
    }

    else if(count >= 1)
    {
      count += 1;
      lastpulse = millis();
    }
    }
  }
  else if (now == prev)
  {
    Serial.println(lastpulse - firstpulse);
    Serial.print("----->Count:");
    Serial.print(count);
    Serial.println("--------------");
    firstpulse = lastpulse;
    lastpulse = 0; 
    count = 0;
  }
}
