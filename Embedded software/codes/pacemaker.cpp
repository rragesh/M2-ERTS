#include "tpl_os.h"

#define APP_Task_heartTask_START_SEC_CODE
#include "tpl_memmap.h"

DeclareEvent(beatEvent);
DeclareEvent(agetEvent);
DeclareEvent(vgetEvent);
DeclareEvent(agetpEvent);
DeclareEvent(vgetpEvent);
DeclareEvent(turnOffLedEvent);
DeclareEvent(aonEvent);
DeclareEvent(vonEvent);
DeclareEvent(aponEvent);
DeclareEvent(vponEvent);
DeclareEvent(asEvent);
DeclareEvent(apEvent);
DeclareEvent(vsEvent);
DeclareEvent(vpEvent);
DeclareEvent(arEvent);
DeclareEvent(tLriElapsed);
DeclareEvent(tPvarpElapsed);
DeclareEvent(tVrpElapsed);
DeclareEvent(tAviElapsed);
DeclareEvent(clkElapsed);

DeclareAlarm(turnOffLedClock);
DeclareAlarm(tLRIClock);
DeclareAlarm(tPVARPClock);
DeclareAlarm(tVRPClock);
DeclareAlarm(tAVIClock);
DeclareAlarm(clk);

DeclareTask(heartTask);
DeclareTask(turnOfLedTask);
DeclareTask(blinkTask);
DeclareTask(lriTask);
DeclareTask(pvarpTask);
DeclareTask(vrpTask);
DeclareTask(aviTask);
DeclareTask(uriTask);

//init PB.3 as output (LED 1 on pin 13).
void initUserLed()
{
    pinMode(7, OUTPUT); /* built in LED */
    pinMode(6, OUTPUT); /* built in LED */
    pinMode(5, OUTPUT); /* built in LED */
    pinMode(4, OUTPUT); /* built in LED */
}

FUNC(int, OS_APPL_CODE) main(void)
{
    initUserLed();
    StartOS(OSDEFAULTAPPMODE);
    return 0;
}

TASK(turnOffLedTask)
{
	while(1){
		int led=7;
		EventMaskType ev;
		WaitEvent(aonEvent|vonEvent|aponEvent|vponEvent);
		GetEvent(turnOffLedTask,&ev);
		ClearEvent(ev);
		if(ev&aonEvent){
			led=7;
		}
		if(ev&vonEvent){
			led=6;
		}
		if(ev&aponEvent){
			led=5;
		}
		if(ev&vponEvent){
			led=4;
		}
		SetRelAlarm(turnOffLedClock,50,0);
		WaitEvent(turnOffLedEvent);
		GetEvent(turnOffLedTask,&ev);
		ClearEvent(ev);
		digitalWrite(led, !digitalRead(led));
		CancelAlarm(turnOffLedClock);
	}
	TerminateTask();
}

TASK(blinkTask)
{
	while(1){
		EventMaskType ev;
		WaitEvent(agetEvent|vgetEvent|agetpEvent|vgetpEvent);
		GetEvent(blinkTask,&ev);
		ClearEvent(ev);
		if(ev&agetEvent){
			digitalWrite(7, !digitalRead(7));
			SetEvent(turnOffLedTask,aonEvent);
		}
		if(ev&vgetEvent){
			digitalWrite(6, !digitalRead(6));
			SetEvent(turnOffLedTask,vonEvent);
		}
		if(ev&agetpEvent){
			digitalWrite(5, !digitalRead(5));
			SetEvent(turnOffLedTask,aponEvent);
		}
		if(ev&vgetpEvent){
			digitalWrite(4, !digitalRead(4));
			SetEvent(turnOffLedTask,vponEvent);
		}
	}
	TerminateTask();
}
TASK(heartTask)
{
	enum{AState,VState} heartState=AState;
	while(1){
		EventMaskType ev;
		WaitEvent(beatEvent|apEvent|vpEvent);
		GetEvent(heartTask,&ev);
		ClearEvent(ev);
		if(ev&beatEvent){
			switch(heartState){
				case AState:
					SetEvent(blinkTask,agetEvent);
					SetEvent(pvarpTask,agetEvent);
					heartState=VState;
					break;
				case VState:
					SetEvent(blinkTask,vgetEvent);
					SetEvent(vrpTask,vgetEvent);
					heartState=AState;
					break;
				default:
					break;
			}
		}
		if(ev&apEvent){
			SetEvent(blinkTask,agetpEvent);
		}
		if(ev&vpEvent){
			SetEvent(blinkTask,vgetpEvent);
		}
	}
	TerminateTask();
}

TASK(lriTask)
{
	enum{LRI,ASed} lriState=LRI;
	SetRelAlarm(tLRIClock,850,0);
	EventMaskType ev;
	while(1)
	{
		switch(lriState){
			case LRI:
				WaitEvent(tLriElapsed|asEvent|vsEvent|vpEvent);
				GetEvent(lriTask,&ev);
				ClearEvent(ev);
				if(ev&tLriElapsed){
					SetEvent(heartTask,apEvent);
				}
				if(ev&asEvent){
					lriState=ASed;
				}
				if(ev&vsEvent || ev&vpEvent){
					CancelAlarm(tLRIClock);
					SetRelAlarm(tLRIClock,850,0);
				}
				break;
			case ASed:
				WaitEvent(vsEvent|vpEvent);
				GetEvent(lriTask,&ev);
				ClearEvent(ev);
				
				CancelAlarm(tLRIClock);
				SetRelAlarm(tLRIClock,850,0);
				lriState=LRI;
				
				break;
			default:
				break;
		}
	}
	TerminateTask();
}

TASK(pvarpTask)
{
	enum{Idle,PVAB,PVARP} pvarpState=Idle;
	EventMaskType ev;
	while(1)
	{
		switch(pvarpState){
			case Idle:
				WaitEvent(vpEvent|vsEvent|agetEvent);
				GetEvent(pvarpTask,&ev);
				ClearEvent(ev);
				if(ev&vpEvent || ev&vsEvent){
					pvarpState=PVAB;
					CancelAlarm(tPVARPClock);
					SetRelAlarm(tPVARPClock,50,0);
				}
				if(ev&agetEvent){
					SetEvent(lriTask,asEvent);
					SetEvent(aviTask,asEvent);
				}
				break;
			case PVAB:
				WaitEvent(tPvarpElapsed);
				GetEvent(pvarpTask,&ev);
				ClearEvent(ev);
				
				CancelAlarm(tPVARPClock);
				SetRelAlarm(tPVARPClock,50,0);
				
				pvarpState=PVARP;
				
				break;
			case PVARP:
				WaitEvent(tPvarpElapsed|agetEvent);
				GetEvent(pvarpTask,&ev);
				ClearEvent(ev);
				if(ev&tPvarpElapsed){
					pvarpState=Idle;
				}
				//if(ev&agetEvent){
				//	SetEvent(someTask,arEvent);
				//}
				break;
			default:
				break;
		}
	}
	TerminateTask();
}

TASK(vrpTask)
{
	enum{Idle,VRP} vrpState=Idle;
	EventMaskType ev;
	while(1)
	{
		switch(vrpState){
			case Idle:
				WaitEvent(vpEvent|vgetEvent);
				GetEvent(vrpTask,&ev);
				ClearEvent(ev);
				if(ev&vpEvent){
					vrpState=VRP;
					CancelAlarm(tVRPClock);
					SetRelAlarm(tVRPClock,150,0);
				}
				if(ev&vgetEvent){
					SetEvent(lriTask,vsEvent);
					SetEvent(pvarpTask,vsEvent);
					SetEvent(aviTask,vsEvent);
					SetEvent(uriTask,vsEvent);
					vrpState=Idle;
				}
				break;
			case VRP:
				WaitEvent(tVrpElapsed);
				GetEvent(vrpTask,&ev);
				ClearEvent(ev);
				
				vrpState=Idle;
				
				break;
			default:
				break;
		}
	}
	TerminateTask();
}

TASK(aviTask)
{
	enum{Idle,AVI,WaitURI} aviState=Idle;
	EventMaskType ev;
	while(1)
	{
		switch(aviState){
			case Idle:
				WaitEvent(asEvent|apEvent);
				GetEvent(aviTask,&ev);
				ClearEvent(ev);
				aviState=AVI;
				CancelAlarm(tAVIClock);
				SetRelAlarm(tAVIClock,150,0);
				break;
			case AVI:
				WaitEvent(tAviElapsed|vsEvent);
				GetEvent(aviTask,&ev);
				ClearEvent(ev);
				if(ev&vsEvent){
					aviState=Idle;
				}
				if(ev&tAviElapsed){
					TickType t;
					StatusType s = GetAlarm(clk,&t);
					if(s==E_OK){
						aviState=WaitURI;
					}
					else{
						SetEvent(lriTask,vpEvent);	
						SetEvent(uriTask,vpEvent);	
						SetEvent(pvarpTask,vpEvent);	
						SetEvent(vrpTask,vpEvent);	
						SetEvent(heartTask,vpEvent);
					}
				}
				break;
			case WaitURI:
				WaitEvent(clkElapsed|vsEvent);
				GetEvent(aviTask,&ev);
				ClearEvent(ev);
				if(ev&vsEvent){
					aviState=Idle;
				}
				if(ev&clkElapsed){
					aviState=Idle;
					SetEvent(lriTask,vpEvent);	
					SetEvent(uriTask,vpEvent);	
					SetEvent(pvarpTask,vpEvent);	
					SetEvent(vrpTask,vpEvent);	
					SetEvent(heartTask,vpEvent);
				}
				break;
			default:
				break;
		}
	}
	TerminateTask();
}

TASK(uriTask)
{
	SetRelAlarm(clk,400,0);
	EventMaskType ev;
	while(1)
	{
		WaitEvent(vsEvent|vpEvent);
		GetEvent(uriTask,&ev);
		ClearEvent(ev);
		CancelAlarm(clk);
		SetRelAlarm(clk,400,0);
	}
	TerminateTask();
}

#define APP_Task_heartTask_STOP_SEC_CODE
#include "tpl_memmap.h"


