.include "m128def.inc"

.def temp = r16 ; temporal registter for some actions
.def temp2 = r17
.def temp3 = r18
.def temp4 = r19
.def tempInter = r20
.def eventsFlags0 = r21
.def eventsFlags1 = r22
.def tempFlag = r23
.def tempFlag2 = r24

.equ speaker_pin_number = 5
.equ speaker_pin_position = 0b00000100 ; speaker will be connected on 5th pin of portD

.equ engine_flap_pin_num = 7 ; engine cooler flap will be contolled by the electrical magnet
.equ engine_flap_pin_pos = 0b10000000

.equ outside_flap_pin_num = 6 ; the same for outside flap
.equ outside_flap_pin_pos = 0b01000000

.equ debug_pin_position = 0b00100000 ; will use this pin to show something idk
.equ debug_pin_number = 5

.equ heater_pin_number = 0
.equ heater_pin_position = 0b000000001
.equ cooler_pin_number = 1
.equ cooler_pin_position = 0b000000010

.equ frontAirbagsTriggerPinNumber = 4
.equ frontAirbagsTriggerPinPosition = 0b00010000
.equ sideAirbagsTriggerPinNumber = 3
.equ sideAirbagsTriggerPinPosition =  0b00001000
.equ maxAllowedSideAcceleration = 40
.equ maxAllowedForwardAcceleration = 100
.equ maxAllowedBackwardAcceleration = 70
.equ accelerationCheckingFreq = 128 + 64 + 32 + 16 + 8 + 4 + 2

.equ insideThermalSensorNum = 0
.equ outsideThermalSensorNum = 1
.equ engineThermalSensorNum = 2
.equ steeringWheelSensorNum = 3
.equ seat0ThermalSensorNum = 4 ; TODO can be done without these 4 lines of code
.equ seat1ThermalSensorNum = 5
.equ seat2ThermalSensorNum = 6
.equ seat3ThermalSensorNum = 7 
.equ seat0FlapControl = 8
.equ seat1FlapControl = 9
.equ seat2FlapControl = 10
.equ seat3FlapControl = 11
.equ accelerationFrontSensorZ = 12
.equ accelerationFrontSensorX = 13
.equ accelerationRearSensorX = 14
.equ lolkekcheburek = 15

.equ tccr0_setup_byte = 0b00000101 ; select speed for timer0
.equ assr_setup_byte =  0b00001000 ; select async oscilator (slower speed and independent from main clock)
.equ tccr1b_setup_byte = 0b00000101 ; seelect speed for timer1
.equ tccr2_setup_byte = 0b00000101 ; select speed for timer2
.equ tccr3b_setup_byte = 0b00000011 ; select speed for timer3
.equ timsk_setup_byte = 0b01000101 ; enable timer2, tiemr1 and timer0 interruption on overflowz
.equ etimsk_setup_byte = 0b00000100 ; enable temer3 interruption on overflow

.equ ch0 = 0
.equ ch1 = 1
.equ ch2 = 2
.equ ch3 = 3
.equ ch4 = 4
.equ ch5 = 5
.equ ch6 = 6
.equ ch7 = 7
.equ ch8 = 8
.equ ch9 = 9
.equ chA = 10
.equ chB = 11
.equ chC = 12
.equ chD = 13
.equ chE = 14
.equ chF = 15
.equ chGrad = 16
.equ chMinus = 17
.equ chR = 18
.equ chSpace = 19

.equ soundSignalsDelay = 128 + 64 + 32 + 16 ; (lower number bigger delays)
	.equ tone0 = 0 + 128 + 64 + 32 + 16 + 8 + 4 ; (lower number -lower tone)
 	.equ tone1 = 0 + 128 + 64 + 32 + 16 + 8 + 4 + 2

.equ displayDataSwitchingFreq = 128 + 64 + 32 ; bigger number - faster switching

.equ checkOutsideTemperatureFreq = 128 + 64 + 32 + 16 

.equ condFanSpeed = 249; higher number - higher speed
	

	; states flags 0
	.equ sf_sound_enabled =				0b10000000
	.equ sf_sound_enabled_n =			7
	.equ sf_warning_signal_enabled =	0b01000000
	.equ sf_warning_signal_enabled_n =  6
	.equ sf_click_signal_enabled  =		0b00100000
	.equ sf_click_signal_enabled_n =	5
	.equ sf_accept_signal_enabled =		0b00010000
	.equ sf_accept_signal_enabled_n =	4
	.equ sf_cancel_signal_enabled =		0b00001000
	.equ sf_cancel_signal_enabled_n =	3
	.equ sf_conditioner_enabled =		0b00000100
	.equ sf_conditioner_enabled_n =		2
	.equ sf_engine_cooler_flap_state =	0b00000010
	.equ sf_engine_cooler_flap_state_n = 1
	.equ sf_display_error_message =		0b00000001
	.equ sf_display_error_message_n =	0

	; eventsFlags0
	.equ ef_update_sound_wave_state =	0b10000000
	.equ ef_update_sound_wave_state_n =	7
	.equ ef_update_sound_type =			0b01000000
	.equ ef_update_sound_type_n =		6
	.equ ef_handle_input =				0b00100000
	.equ ef_handle_input_n =			5
	.equ ef_update_7seg_screen =		0b00010000
	.equ ef_update_7seg_screen_n =		4
	.equ ef_read_analog_sensors =		0b00001000
	.equ ef_read_analog_sensors_n =		3
	.equ ef_save_target_temperature =	0b00000100
	.equ ef_save_target_temperature_n = 2
	.equ ef_update_heater =				0b00000010
	.equ ef_update_heater_n =			1
	.equ ef_update_conditioner_fan =	0b00000001
	.equ ef_update_conditioner_fan_n =	0


	; events flags1
	.equ ef_load_target_temp =			0b10000000
	.equ ef_load_target_temp_n =		7
	.equ ef_update_program_timers =		0b01000000
	.equ ef_update_program_timers_n =	6
	.equ ef_update_flaps =				0b00100000
	.equ ef_update_flaps_n =			5
	.equ ef_increment_tachometer =		0b00010000
	.equ ef_increment_tachometer_n =	4
	.equ ef_update_tachometer =			0b00001000
	.equ ef_update_tachometer_n =		3
	.equ ef_check_engine_temperature =	0b00000100
	.equ ef_check_engine_temperature_n = 2
	.equ ef_switch_display_data =		0b00000010
	.equ ef_switch_display_data_n =		1
	.equ ef_handle_current_key =		0b00000001
	.equ ef_handle_current_key_n =		0

	; events falgs 2
	.equ ef_check_outside_temperature =		0b10000000
	.equ ef_check_outside_temperature_n =   7
	.equ ef_check_acceleration =			0b01000000
	.equ ef_check_acceleration_n =			6


//addreses in eeprom where usefull data will be saved
.equ targetTempSavingAddr = 0


.dseg
	analogValuesTable:
		.byte 16 ; there will be stored all the data from all 8 thermal sensors
	SevenSegScrBuff:
		.byte 8 ; reserve 8 bytes for 7 segment dysplay buffer
	currentSensorNum:
		.byte 1 ; there will be stored number of thermalSensor we wanna save data from
	curPos7Seg:
		.byte 1 ; position of currently printing glyph on 7seg display
	
	currentDisplayData:
		.byte 1 ; 0 - temperatures, 1 - tachometer
	switchDisplayDataTimerH:
		.byte 1 ;
	switchDisplayDataTimerL:
		.byte 1 ;

	targetTemperature:
		.byte 1 ; reserve  1 byte to keep temperature while running\
	updateConditionerTimer:
		.byte 1
	flapsUpdateTimer:
		.byte 1
	currentControlledFlap:
		.byte 1
	conditionerFanTimer0:
		.byte 1 ; reserve 2 bytes to store current conditioner fan timer
	conditionerFanTimer1:
		.byte 1
	conditionerFanState:
		.byte 1 ; reserve 1 byte to know fan "rotation" (not quite but who cares)
	checkOutsideTemperatureTimerH:
		.byte 1
	checkOutsideTemperatureTimerL:
		.byte 1

	tachometerTimerH:
		.byte 1
	tachometerTimerL:
		.byte 1
	tachometerCounter:
		.byte 1
	engineSpeed:
		.byte 1
	checkEngineTemperatureTimer:
		.byte 1

	currentTone:
		.byte 1 ; there will be stored current note of sound

	currentKey:
		.byte 1 ; there will be storred a number of the last pressed key
	handleCurrentKeyTimer:
		.byte 1

	checkAccelerationTimer:
		.byte 1 

	eventsFlags2:
		.byte 1
	statesFlags0:
		.byte 1

.cseg
	.org 0
		jmp reset

	
		.org $0002
			jmp extInt0Handler
		.org $0004
			jmp extInt1Handler
		.org $0014
			jmp timer2OvfHandler
		.org $001C
			jmp timer1OvfHandler
		.org $0020
			jmp timer0OvfHandler
		.org $002a
			jmp adcConvertionHandler
		.org $003a 
			jmp timer3OvfHandler


	.org $0050 ; start program almost right after all intrruptions vectors

	charTable7seg: 
		//main simbols for 7seg display
		.db 0b00000011 /*0*/, 0b10011111 /*1*/, 0b00100101 /*2*/, 0b00001101 /*3*/
		.db	0b10011001 /*4*/, 0b01001001 /*5*/, 0b01000001 /*6*/, 0b00011111 /*7*/
		.db	0b00000001 /*8*/, 0b00001001 /*9*/, 0b00010001 /*a*/, 0b11000001 /*b*/
		.db	0b01100001 /*c*/, 0b10000101 /*d*/, 0b01100001 /*e*/, 0b01110001 /*f*/

		//extra simbols for 7seg display
		.db	0b00111001 /*grad*/, 0b11111101 /*minus*/, 0b01110011 /*r*/, 0xff /*space*/

	errorString7: 
		.db chE, chR, chR, chSpace
	

	reset:

		ldi temp, high(ramend) ; setting up stack pointer
		out sph, temp
		ldi temp, low(ramend)
		out spl, temp

		; setting up ports
		ldi temp, 0b10011000 ; 0,1,2-in (read column), 3,4 - out (select row), 7 - out (check all buttons)
		out ddra, temp
		ldi temp, 0b10000000 ; enable continuous check if any button is pressed
		out porta, temp

		ldi temp, 0b11100000 ; set last hree bits of portB to be output
		out ddrb, temp

		ldi temp, 0b11111111 ; set all pins of portC to be output
		out ddrc, temp

		ldi temp, speaker_pin_position
		ldi temp2, engine_flap_pin_pos
		or temp, temp2
		ldi temp2, outside_flap_pin_pos
		or temp, temp2
		out ddrd, temp

		ldi temp, 0xff ;heater, cooler, 2,3 - flap state, 4-7 step engines (fans)
		out ddre, temp

		ldi temp, 0b11111110 ; 0 - thermSensors in, 1-4 selected thermalSensor, 6-7 selected flap
		sts ddrf, temp


		//setting up external interrupts
		ldi temp, 0b00000011 ; enable external intrruption int0, int1
		out eimsk, temp

		ldi temp, 0b00001111 ; set ext interrupt0 and interrupt1 on rising edge
		sts eicra, temp

		//setting up timers
		ldi temp, tccr0_setup_byte
		out tccr0, temp ; set timer0 speed
		ldi temp, assr_setup_byte
		out assr, temp ; select async oscilator ( timer0 will be super slow and independent from main clock)

		ldi temp, tccr1b_setup_byte ; setting up timer1 speed
		out tccr1b, temp;
		ldi temp, 0xff ; reset timer
		ldi temp2, 0x00
		out tcnt1h, temp
		out tcnt1l, temp2

		ldi temp, tccr2_setup_byte
		out tccr2, temp ; set timer2 speed

		ldi temp, tccr3b_setup_byte ; setting up timer3 speed
		sts tccr3b, temp

		ldi temp, timsk_setup_byte
		out timsk, temp ; enable interupts on timers overflowing

		ldi temp, etimsk_setup_byte
		sts etimsk, temp


		ldi temp, 0b01100000 ; use external cap in adc, left adjust the result
		out admux, temp

		ldi temp, 0b11011111 ; en adc, start conv, , clear int flag, enable interuptions, setup prescaler
		out adcsra, temp

	
		sbr eventsFlags1, ef_load_target_temp ; try to load target temp on start

		sei ;  allow interrupts


	backgroundProcess:
		sbrc eventsFlags0, ef_update_sound_type_n
			call updateSoundType
		sbrc eventsFlags0, ef_update_sound_wave_state_n
			call updateWaveState
		sbrc eventsFlags0, ef_handle_input_n
			call readInput
		sbrc eventsFlags0, ef_update_7seg_screen_n
			call update7segScreen
		sbrc eventsFlags0, ef_read_analog_sensors_n
			call readAnalogSensors
		sbrc eventsFlags0, ef_save_target_temperature_n
			call saveTargetTemperature
		sbrc eventsFlags0, ef_update_heater_n
			call updateConditioner
		sbrc eventsFlags0, ef_update_conditioner_fan_n
			call updateConditionerFan

		sbrc eventsFlags1, ef_load_target_temp_n
			call loadTargetTemperature
		sbrc eventsFlags1, ef_update_program_timers_n
			call updateProgramTimers 
		sbrc eventsFlags1, ef_update_flaps_n
			call updateFlaps
		sbrc eventsFlags1, ef_update_tachometer_n
			call updateTachometer
		sbrc eventsFlags1, ef_increment_tachometer_n
			call incrementTachometerCounter
		sbrc eventsFlags1, ef_check_engine_temperature_n
			call checkEngineTemperature
		sbrc eventsFlags1, ef_switch_display_data_n
			call switchDisplayData
		sbrc eventsFlags1, ef_handle_current_key_n
			call handleCurrentKey

		ldi xh, high(eventsFlags2)
		ldi xl, low(eventsFlags2) 
		ld tempFlag, x
		sbrc tempFlag, ef_check_outside_temperature_n
			call checkOutsideTemperature
		sbrc tempFlag, ef_check_acceleration_n
			call checkAcceleration

 	jmp backgroundProcess

	checkAcceleration:
		cbr tempFlag, ef_check_acceleration
		sts eventsFlags2, tempFlag

		ldi xh, high(analogValuesTable)
		ldi xl, low(analogValuesTable)
		ldi temp, accelerationFrontSensorZ
		add xl, temp
		ld temp2, x

		cpi temp2, 128 - maxAllowedBackwardAcceleration
			brlo frontCollision
		
		checkRearCollision:
			cpi temp2, 128 + maxAllowedForwardAcceleration
				brsh rearCollision

		

		checkFrontLeftCollision:
		ldi xh, high(analogValuesTable)
		ldi xl, low(analogValuesTable)
		ldi temp, accelerationFrontSensorX
		add xl, temp
		ld temp3, x

		
		cpi temp3, 128 + maxAllowedSideAcceleration
			brsh frontLeftCollision
		
		checkFrontRightCollision:
			cpi temp3, 128 - maxAllowedSideAcceleration
				brlo frontRightCollision

		checkBackLeftCollision:
		ldi xh, high(analogValuesTable)
		ldi xl, low(analogValuesTable)
		ldi temp, accelerationRearSensorX
		add xl, temp
		ld temp4, x

		cpi temp4, 128 + maxAllowedSideAcceleration
			brsh backLeftCollision

		checkBackRightCollision:
			cpi temp4, 128 - maxAllowedSideAcceleration
				brlo backRightCollision
				jmp endCheckCollision

		frontCollision:
			call enableWarningSignal
			jmp checkRearCollision

		rearCollision:
			sbi portd, frontAirbagsTriggerPinNumber
			call enableWarningSignal
			jmp checkFrontLeftCollision

		frontLeftCollision:
			sbi portd, sideAirbagsTriggerPinNumber
			call enableWarningSignal
			jmp checkFrontRightCollision

		frontRightCollision:
			sbi portd, sideAirbagsTriggerPinNumber
			call enableWarningSignal
			jmp checkBackLeftCollision

		backLeftCollision:
			sbi portd, sideAirbagsTriggerPinNumber
			call enableWarningSignal
			jmp checkBackRightCollision

		backRightCollision:
			sbi portd, sideAirbagsTriggerPinNumber
			call enableWarningSignal

		endCheckCollision:
	ret

	handleCurrentKey:
		cbr eventsFlags1, ef_handle_current_key
		ldi xh, high(currentKey)
		ldi xl, low(currentKey)
		ld temp, x
		cpi temp, 28
			breq callRizeTemperature
		cpi temp, 20
			breq callLowerTemperature
		cpi temp, 12
			breq callHandleCancelKey
		cpi temp, 25
			breq callSwitchConditioner
			jmp handleCurrentKeyEnd

		callRizeTemperature:
			call rizeTemperature
			jmp handleCurrentKeyEnd

		callLowerTemperature:
			call lowerTemperature
			jmp handleCurrentKeyEnd

		callHandleCancelKey:
			call handleCancelKey
			jmp handleCurrentKeyEnd

		callSwitchConditioner:
			call switchConditioner

		handleCurrentKeyEnd:
	ret


	handleCancelKey:
		ldi zh, high(statesFlags0)
		ldi zl, low(statesFlags0)
		ld tempFlag2, z
		sbrc tempFlag2, sf_warning_signal_enabled_n
			jmp cancelWarning
			jmp endHandleCancelKey

		cancelWarning:
			ldi temp, 0
			sts currentKey, temp ; current key is handled. so clear it
			call disableWarningSignal
			call enableCancelSignal

		endHandleCancelKey:

	ret

	resetSoundState:
		cbr tempFlag2, sf_click_signal_enabled
		cbr tempFlag2, sf_accept_signal_enabled
		cbr tempFlag2, sf_cancel_signal_enabled
		cbr tempFlag2, sf_warning_signal_enabled
		cbr tempFlag2, sf_sound_enabled
	ret

	enableWarningSignal:
		ldi zh, high(statesFlags0)
		ldi zl, low(statesFlags0)
		ld tempFlag2, z
		sbrc tempFlag2, sf_warning_signal_enabled_n
			ret ; return emmediately if it is already enabled
		call resetSoundState
		sbr tempFlag2, sf_warning_signal_enabled
		sts statesFlags0, tempFlag2
	ret

	disableWarningSignal:
		ldi zh, high(statesFlags0)
		ldi zl, low(statesFlags0)
		ld tempFlag2, z
		cbr tempFlag2, sf_warning_signal_enabled
		sts statesFlags0, tempFlag2
	ret

	enableClickSignal:
		ldi zh, high(statesFlags0)
		ldi zl, low(statesFlags0)
		ld tempFlag2, z
		call resetSoundState
		sbr tempFlag2, sf_click_signal_enabled
		sts statesFlags0, tempFlag2
	ret

	enableAcceptSignal:
		ldi zh, high(statesFlags0)
		ldi zl, low(statesFlags0)
		ld tempFlag2, z
		call resetSoundState
		sbr tempFlag2, sf_accept_signal_enabled
		sts statesFlags0, tempFlag2
	ret

	enableCancelSignal:
		ldi zh, high(statesFlags0)
		ldi zl, low(statesFlags0)
		ld tempFlag2, z
		call resetSoundState
		sbr tempFlag2, sf_cancel_signal_enabled
		sts statesFlags0, tempFlag2
	ret

	updateProgramTimers:
		ldi temp, 0xff ; preset timer to change overflowing freq
		ldi temp2, 0xf0
		sts tcnt3h, temp
		sts tcnt3l, temp2

		cbr eventsFlags1, ef_update_program_timers

		ldi zh, high(statesFlags0)
		ldi zl, low(statesFlags0)
		ld tempFlag2, z
		sbrs tempFlag2, sf_conditioner_enabled_n ; if conditioner is not enabled then return emmidiately
			jmp updTachTimer
		
		ldi xh, high(updateConditionerTimer) ; get timer from ram
		ldi xl, low(updateConditionerTimer)
		ld temp, x
		inc temp
		cpi temp, 255 ; emulate ovf interruption lol kek ahhahaha :-)))))))
			breq setConditionerFlag
			jmp saveConditionerTimer

		setConditionerFlag:
			sbr eventsFlags0, ef_update_heater ; create "update_heater" task

			ldi temp, 0 ; reset heater update timer

		saveConditionerTimer:
			sts updateConditionerTimer, temp ;and save it to ram

		; CONDITIONER FAN ;
		ldi xh, high(conditionerFanTimer0) ; get temperature from ram
		ldi xl, low(conditionerFanTimer0)
		ld temp, x
		cpi temp, 255 
			breq checkCondFanTimer1	
			inc temp
			sts conditionerFanTimer0, temp
			jmp endUpdateCondFanTimer

		checkCondFanTimer1:
			ldi temp, 128 + 64
			sts conditionerFantimer0, temp
			ldi xh, high(conditionerFanTimer1)
			ldi xl, low(conditionerFanTimer1)
			ld temp, x

			cpi temp, 255
				breq setUpdateCondFanFlag
				inc temp
				sts conditionerFanTimer1, temp
				jmp endUpdateCondFanTimer
				
				

		setUpdateCondFanFlag:
			sbr eventsFlags0, ef_update_conditioner_fan ; start upate fan task

			ldi temp, 128 + 64
			sts conditionerFanTimer0, temp ; reset timer
			ldi temp, condFanSpeed
			sts conditionerFanTimer1, temp
		
		endUpdateCondFanTimer:



		////////////udpate flaps timer ///////////
		ldi xh, high(flapsUpdateTimer) ; get flaps timer from ram
		ldi xl, low(flapsUpdateTimer)
		ld temp, x
		inc temp
		cpi temp, 255 ; emulate ovf interruption lol kek ahhahaha :-)))))))
			breq setUpdateFlapsFlag
			jmp saveUpdateFlapsTimer

		setUpdateFlapsFlag:
			sbr eventsFlags1, ef_update_flaps ; create "update_flaps" task

			ldi temp, 0 ; reset flaps update timer

		saveUpdateFlapsTimer:
			sts flapsUpdateTimer, temp ;and save it to ram


		
		updTachTimer:
		ldi xh, high(tachometerTimerL)
		ldi xl, low(tachometerTimerL)
		ld temp, x
		inc temp
		cpi temp, 255 
			breq incrementTachTimerH
			jmp saveTachTimerL

		incrementTachTimerH:
			ldi temp, 0
			sts tachometerTimerL, temp

			ldi xh, high(tachometerTimerH)
			ldi xl, low(tachometerTimerH)
			ld temp, x
			inc temp
			cpi temp, 255
				breq resetTachTimer
				jmp saveTachTimerH

		saveTachTimerL:
			sts tachometerTimerL, temp
			jmp endTachTimerUpdate

		saveTachTimerH:
			sts tachometerTimerH, temp
			jmp endTachTimerUpdate

		resetTachTimer:
			ldi temp, 128 + 64 + 32 + 16 + 8 + 4
			sts tachometerTimerH, temp
			ldi temp, 0
			sts tachometerTimerL, temp
			
			sbr eventsFlags1, ef_update_tachometer
		
		endTachTimerUpdate:


		//////////// switch displayed data timer ///////////////////
		ldi xh, high(switchDisplayDataTimerL) ; get timer from ram
		ldi xl, low(switchDisplayDataTimerL)
		ld temp, x
		inc temp
		cpi temp, 255 ; emulate ovf interruption lol kek ahhahaha :-)))))))
			breq incSwitchDisplayDataTimerH
			sts switchDisplayDataTimerL, temp
			jmp endDisplayDataTimerUpdate

		incSwitchDisplayDataTimerH:
			ldi temp, 0
			sts switchDisplayDataTimerL, temp

			ldi xh, high(switchDisplayDataTimerH)
			ldi xl, low(switchDisplayDataTimerH)
			ld temp, x
			inc temp
			cpi temp, 255
				breq setSwitchDisplayDataFlag
				jmp saveSwitchDisplayDataTimerH

			setSwitchDisplayDataFlag:
				sbr eventsFlags1, ef_switch_display_data

				resetDisplayDataTimerH:
				ldi temp, displayDataSwitchingFreq ; reset  timer

			saveSwitchDisplayDataTimerH:
				sts switchDisplayDataTimerH, temp ;and save it to ram
		
		endDisplayDataTimerUpdate:


		
		////////////udpate check engine temperature timer ///////////
		ldi xh, high(checkEngineTemperatureTimer) ; get flaps timer from ram
		ldi xl, low(checkEngineTemperatureTimer)
		ld temp, x
		inc temp
		cpi temp, 255 ; emulate ovf interruption lol kek ahhahaha :-)))))))
			breq setCheckEngineTemperatureFlag
			jmp saveCheckEngineTemperatureTimer

		setCheckEngineTemperatureFlag:
			sbr eventsFlags1, ef_check_engine_temperature ; create "update_flaps" task

			ldi temp, 0 ; reset flaps update timer

		saveCheckEngineTemperatureTimer:
			sts checkEngineTemperatureTimer, temp ;and save it to ram

		endCheckEngineTemperatureTimerUpdate:

		//////// handle the last pressed key timer //////////
		ldi xh, high(handleCurrentKeyTimer)
		ldi xl, low(handleCurrentKeyTimer)
		ld temp, x
		inc temp
		cpi temp, 254
			brsh setHandleCurrentKeyFlag
			jmp saveHandleCurrentKeyTimer

		setHandleCurrentKeyFlag:
			sbr eventsFlags1, ef_handle_current_key
			ldi temp, 0

		saveHandleCurrentKeyTimer:
			sts handleCurrentKeyTimer, temp

		endHandleCrrentKeyTimerUpdate:


		////////// outside temp checking timer//////////
		ldi xh, high(eventsFlags2)
		ldi xl, low(eventsFlags2)
		ld tempFlag, x
		ldi xh, high(checkOutsideTemperatureTimerL) ; get timer from ram
		ldi xl, low(checkOutsideTemperatureTimerL)
		ld temp, x
		inc temp
		cpi temp, 255 ; emulate ovf interruption lol kek ahhahaha :-)))))))
			breq incCheckOutsideTemperatureTimerH
			sts checkOutsideTemperatureTimerL, temp
			jmp endCheckOutsideTemperatreTimerUpdate

		incCheckOutsideTemperatureTimerH:
			ldi temp, 0
			sts checkOutsideTemperatureTimerL, temp

			ldi xh, high(CheckOutsideTemperatureTimerH)
			ldi xl, low(CheckOutsideTemperatureTimerH)
			ld temp, x
			inc temp
			cpi temp, 255
				breq setCheckOutsideTemperatreFlag
				jmp saveCheckOutsideTemperatreTimerH

			setCheckOutsideTemperatreFlag:
				sbr tempFlag, ef_check_outside_temperature
				sts eventsFlags2, tempFlag

				resetCheckOutsideTemperatreTimerH:
				ldi temp, checkOutsideTemperatureFreq ; reset  timer

			saveCheckOutsideTemperatreTimerH:
				sts CheckOutsideTemperatureTimerH, temp ;and save it to ram
		
		endCheckOutsideTemperatreTimerUpdate:


		//////// check accelerations timer //////////
		ldi xh, high(checkAccelerationTimer)
		ldi xl, low(checkAccelerationTimer)
		ld temp, x
		inc temp
		cpi temp, 254
			brsh setCheckAccelerationFlag
			jmp saveCheckAccelerationTimer

		setCheckAccelerationFlag:
			sbr tempFlag, ef_check_acceleration
			sts eventsFlags2, tempFlag
			ldi temp, 0

		saveCheckAccelerationTimer:
			sts checkAccelerationTimer, temp

		endCheckAccelerationUpdate:


	ret


	rizeTemperature:
		ldi zh, high(statesFlags0)
		ldi zl, low(statesFlags0)
		ld tempFlag2, z
		sbrs tempFlag2, sf_conditioner_enabled_n ; if conditioner is not enabled then return emmidiately
			ret
		
		call enableClickSignal ; make click sound
		ldi temp, 0
		sts currentKey, temp
		ldi xh, high(targetTemperature) ; get temperature from ram
		ldi xl, low(targetTemperature)
		ld temp, x
		inc temp ; increment temperature
		
		cpi temp, 31 ; limit temperature to 30 grad
			brlo saveTemperatureToRam
			ldi temp, 30

		saveTemperatureToRam:
			sts targetTemperature, temp ;and save it to ram

		sbr eventsFlags0, ef_save_target_temperature
	ret

	lowerTemperature:
		ldi zh, high(statesFlags0)
		ldi zl, low(statesFlags0)
		ld tempFlag2, z
		sbrs tempFlag2, sf_conditioner_enabled_n ; if conditioner is not enabled then return emmidiately
			ret
		
		call enableClickSignal ; make click sound

		ldi temp, 0
		sts currentKey, temp
		ldi xh, high(targetTemperature) ; get temperature from ram
		ldi xl, low(targetTemperature)
		ld temp, x
		dec temp ; decrement temperature

		cpi temp, 10 ; limit temperature to 30 grad
			brge saveTemperatureToRam
			ldi temp, 10

		sts targetTemperature, temp ;and save it to ram

		sbr eventsFlags0, ef_save_target_temperature
	ret

	switchConditioner:
		ldi temp, 0
		sts currentKey, temp
		ldi zh, high(statesFlags0)
		ldi zl, low(statesFlags0)
		ld tempFlag2, z
		sbrs tempFlag2, sf_conditioner_enabled_n
			jmp cond_en
			jmp cond_dis

		cond_en:
			sbr tempFlag2, sf_conditioner_enabled
			sts statesFlags0, tempFlag2
			call enableAcceptSignal
			jmp endSwitchConditioner
		cond_dis:
			cbr tempFlag2, sf_conditioner_enabled
			sts statesFlags0, tempFlag2
			call enableCancelSignal
			cbi porte, heater_pin_number
			cbi porte, cooler_pin_number
			jmp endSwitchConditioner
		endSwitchConditioner:
	ret



	saveTargetTemperature:
		sbic eecr, eewe ; wait for completion of previous write
			jmp endWritingAttempt
		ldi temp, targetTempSavingAddr ; setup address where to wirte
		out eearh, temp
		out eearl, temp
		
		ldi xh, high(targetTemperature) ; get temperature from ram
		ldi xl, low(targetTemperature)
		ld temp, x

		out eedr, temp ; store data to data register
		sbi eecr, eemwe ; write logical one to eemwe
		sbi eecr, eewe ; start eeprom write by setting eewe

		cbr eventsFlags0, ef_save_target_temperature ; end writing task

		endWritingAttempt:

	ret

	loadTargetTemperature:
		sbic eecr, eewe ; wait for completion of previous write
			jmp endReadAttempt
		ldi temp, targetTempSavingAddr
		out eearh, temp
		out eearl, temp
		sbi  eecr, eere ; start eeprom read by writing eere
		in temp, eedr ; read data from data register
		sts targetTemperature, temp

		cbr eventsFlags1, ef_load_target_temp ; end loading task if it success
		
		endReadAttempt:
	ret

	checkEngineTemperature:
		cbr eventsFlags1, ef_check_engine_temperature
		ldi xh, high(analogValuesTable) ; get engine temperature from ram
		ldi xl, low(analogValuesTable)
		ldi temp4, engineThermalSensorNum
		add xl, temp4
		ld temp, x

		cpi temp, 128 + 120 ; if engine temperature is more than 120C
			brsh enableEngineTempWarning
		cpi temp, 128 - 80 ; if enginge temperature is strangely low then warning too
			brlo enableEngineTempWarning
			jmp endEngineTemperatureCheck
			
		enableEngineTempWarning:
			call enableWarningSignal

		endEngineTemperatureCheck:
	ret

	checkOutsideTemperature:
		cbr tempFlag, ef_check_outside_temperature
		sts eventsFlags2, tempFlag

		ldi xh, high(analogValuesTable) ; get engine temperature from ram
		ldi xl, low(analogValuesTable)
		ldi temp4, outsideThermalSensorNum
		add xl, temp4
		ld temp, x

		cpi temp, 128 + 70 ; if  temperature is more then 70C than warning
			brsh enableOutsideTempWarning
		cpi temp, 128 - 80 ; if temperature lower then -80 than warning
			brlo enableOutsideTempWarning
			jmp endOutsideTemperatureCheck
			
		enableOutsideTempWarning:
			call enableWarningSignal

		endOutsideTemperatureCheck:
	ret

	updateConditioner:
		ldi zh, high(statesFlags0)
		ldi zl, low(statesFlags0)
		ld tempFlag2, z
		sbrs tempFlag2, sf_conditioner_enabled_n ; if conditioner is not enabled then return emmidiately
			ret
		cbr eventsFlags0, ef_update_heater

		ldi xh, high(targetTemperature) ; get target temperature from ram
		ldi xl, low(targetTemperature)
		ld temp, x

		cpi temp, 31
			brsh condTemperatureWarning
		cpi temp, 10
			brlo condTemperatureWarning

		ldi xh, high(analogValuesTable) ; get current temperature from ram
		ldi xl, low(analogValuesTable)
		ld temp2, x

		

		ldi xh, high(analogValuesTable) ; get outside temperature from ram
		ldi xl, low(analogValuesTable)
		ldi temp4, outsideThermalSensorNum
		add xl, temp4
		ld temp4, x

		cpi temp2, 128 + 70 ; + 70C
			brsh condTemperatureWarning
		cpi temp2, 128 - 80
			brlo condTemperatureWarning ; -80C
		jmp updateHeater

		condTemperatureWarning:
			call enableWarningSignal

		updateHeater:
		cpi temp2, 128 ; if temperature is minus then enable heater emmediately 
			brlo enableHeater
			subi temp2, 128
		
		cp temp2, temp
			brlo enableHeater
			jmp disableHeater

		disableHeater: ; ye ye ye useless, but just for code readability
			cbi porte, heater_pin_number
			sbi porte, cooler_pin_number
			jmp endUpdateConditioner

		enableHeater:
			sbi porte, heater_pin_number
			cbi porte, cooler_pin_number

		; TODO add hummidity sensor and regulate it

		endUpdateConditioner: 
		call resolveEngineFlap
		call resolveOutsideFlap
	ret

	resolveEngineFlap:
		ldi xh, high(analogValuesTable) ; get current temperature from ram
		ldi xl, low(analogValuesTable)
		ld temp, x

		ldi xh, high(analogValuesTable) ; get engine temperature from ram
		ldi xl, low(analogValuesTable)
		ldi temp4, engineThermalSensorNum
		add xl, temp4
		ld temp2, x	

		cp temp, temp2
			brlo checkIfNeedWarmup
			jmp checkIfNeedCooldown

		checkIfNeedCooldown:
			sbic porte, cooler_pin_number
				jmp openEngineFlap
				jmp closeEngineFlap

		checkIfNeedWarmup:
			sbic porte, heater_pin_number
				jmp openEngineFlap
				jmp closeEngineFlap

		openEngineFlap:
			sbi portd, engine_flap_pin_num
			jmp endResolveEngineFlap

		closeEngineFlap:
			cbi portd, engine_flap_pin_num

		endResolveEngineFlap:
	ret

	resolveOutsideFlap:
		ldi xh, high(analogValuesTable) ; get current temperature from ram
		ldi xl, low(analogValuesTable)
		ld temp, x

		ldi xh, high(analogValuesTable) ; get outside temperature from ram
		ldi xl, low(analogValuesTable)
		ldi temp4, outsideThermalSensorNum
		add xl, temp4
		ld temp2, x	

		cp temp, temp2
			brlo checkIfNeedWarmupFromOutside
			jmp checkIfNeedCooldownFromOutside

		checkIfNeedCooldownFromOutside:
			sbic porte, cooler_pin_number
				jmp openOutsideFlap
				jmp closeOutsideFlap

		checkIfNeedWarmupFromOutside:
			sbic porte, heater_pin_number
				jmp openOutsideFlap
				jmp closeOutsideFlap

		openOutsideFlap:
			sbi portd, outside_flap_pin_num
			jmp endResolveOutsideFlap

		closeOutsideFlap:
			cbi portd, outside_flap_pin_num

		endResolveOutsideFlap:

	ret

	updateConditionerFan:
		ldi zh, high(statesFlags0)
		ldi zl, low(statesFlags0)
		ld tempFlag2, z
		sbrs tempFlag2, sf_conditioner_enabled_n ; if conditioner is not enabled then return emmidiately
			ret
		cbr eventsFlags0, ef_update_conditioner_fan

		ldi xh, high(conditionerFanState) ; get fan state from ram
		ldi xl, low(conditionerFanState)
		ld temp, x
		cpi temp, 0
			breq resetCondFanState

		lsr temp
		cpi temp, 0b00001000 ; if we are trying to lightup pin not connected to fan then reset it 
			breq resetCondFanState
			jmp saveCondFanState
		resetCondFanState:
			ldi temp, 0b10000000
		saveCondFanState:
			sts conditionerFanState, temp ;and save it to ram	
			in temp2, porte ; get current state of prote (to not broke other stuff connected to this port)
			andi temp2, 0b00001111
			or temp2, temp ; out new fan state to porte
			out porte, temp2
	ret

	updateFlaps:
		ldi zh, high(statesFlags0)
		ldi zl, low(statesFlags0)
		ld tempFlag2, z
		sbrs tempFlag2, sf_conditioner_enabled_n ; if conditioner is not enabled then return emmidiately
			ret
		cbr eventsFlags1, ef_update_flaps

		ldi zh, high(currentControlledFlap) ; get the number of a flap we wanna update now
		ldi zl, low(currentControlledFlap)
		ld temp, z

		ldi xh, high(analogValuesTable) ; get current state of needed flap control from ram
		ldi xl, low(analogValuesTable)
		ldi temp3, 8 ; flaps controlls data begins from 8th byte
		add xl, temp3 
		add xl, temp ; ofset to current flap control byte
	
		ld temp2, x

		andi temp2, 0b11000000 ; leave only two most significant byts (every falp will have only 4 possible states)
		swap temp2

		in temp3, pine ; get the contents from porte
		andi temp3, 0b11110011 ; clear two pins controlling the flaps to rewrite them in next two commands
		or temp3, temp2 ; and combine it with the date we wanno output
		out porte, temp3 ; then... output

		in temp3, pinf ; the same trick with two pins of portf (selecting one of our 4 flaps)
		andi temp3, 0b00111111 
		mov temp2, temp
		swap temp2
		lsl temp2
		lsl temp2
		or temp3, temp2
		sts portf, temp3

		inc temp
		
		cpi temp, 4 ; if next selected falp as "non existing" 5th flap then fump to first one 
			brsh jumpToFirstFlap
			jmp saveNextFlapNum
		
		jumpToFirstFlap:
			ldi temp, 0

		saveNextFlapNum:
			sts currentControlledFlap, temp
	ret


	updateSoundType:
		cbr eventsFlags0, ef_update_sound_type

		ldi temp, soundSignalsDelay ; reset timer immediately
		out tcnt0, temp
		
		ldi zh, high(statesFlags0)
		ldi zl, low(statesFlags0)
		ld tempFlag2, z
		sbrc tempFlag2, sf_warning_signal_enabled_n
			jmp updateWarningSignal ; if warning signal is enabled then update it
		sbrc tempFlag2, sf_click_signal_enabled_n
			jmp updateClickSignal
		sbrc tempFlag2, sf_accept_signal_enabled_n
			jmp updateAcceptSignal
		sbrc tempFlag2, sf_cancel_signal_enabled_n
			jmp updateCancelSignal
		ret	

		updateWarningSignal:	
			sbrs tempFlag2, sf_sound_enabled_n
				jmp warningSound_case_0
				jmp warningSound_case_1
				warningSound_case_0:
					ldi temp, tone0
					sts currentTone, temp ; set sound tone
					sbr tempFlag2, sf_sound_enabled ; enable sound
					sts statesFlags0, tempFlag2
				ret

				warningSound_case_1:
					cbr tempFlag2, sf_sound_enabled ; disable sound
					sts statesFlags0, tempFlag2
				ret

		updateClickSignal:
			sbrs tempFlag2, sf_sound_enabled_n
				jmp clickSound_case_0 ; if we haven't clicked yet than click
				jmp clickSound_case_1 ; otherwise end lcick signal

				clickSound_case_0:
					ldi temp, tone0
					sts currentTone, temp ; set low sound tone  
					sbr tempFlag2, sf_sound_enabled ; enable sound
					sts statesFlags0, tempFlag2
				ret

				clickSound_case_1:
					cbr tempFlag2, sf_click_signal_enabled ; end click sound
					cbr tempFlag2, sf_sound_enabled ; disable sound
					sts statesFlags0, tempFlag2
				ret

		updateAcceptSignal:
			sbrs tempFlag2, sf_sound_enabled_n
				jmp acceptSound_case0 ; if sound is not enabled yet than enable it and set tone low
				jmp acceptSound_case1 ; otherwise if tone low then make it high. if tone high than end accept signal

				acceptSound_case0:	
					ldi temp, tone0		
					sts currentTone, temp ; set low sound tone  
					sbr tempFlag2, sf_sound_enabled ; enable sound
					sts statesFlags0, tempFlag2
				ret

				acceptSound_case1:
					ldi xh, high(currentTone)
					ldi xl, low(currentTone)
					ld temp, x

					cpi temp, tone0
						breq acceptSound_case2 ; if tone is low make it high
						jmp acceptSound_case3 ; otherwise end signal
						acceptSound_case2:
							ldi temp, tone1
							sts currentTone, temp ; set high sound tone  
						ret
						acceptSound_case3:
							cbr tempFlag2, sf_sound_enabled	; disable sound
							cbr tempFlag2, sf_accept_signal_enabled ; end accept signal
							sts statesFlags0, tempFlag2
						ret

		updateCancelSignal:
			sbrs tempFlag2, sf_sound_enabled_n
				jmp cancelSound_case0 ; if sound is not enabled yet than enable it and set tone low
				jmp cancelSound_case1 ; otherwise if tone low then make it high. if tone high than end accept signal

				cancelSound_case0:
					ldi temp, tone1			
					sts currentTone, temp ; set high sound tone  
					sbr tempFlag2, sf_sound_enabled ; enable sound
					sts statesFlags0, tempFlag2
				ret

				cancelSound_case1:
					ldi xh, high(currentTone)
					ldi xl, low(currentTone)
					ld temp, x

					cpi temp, tone1
						breq cancelSound_case2 ; if tone high make it low
						jmp cancelSound_case3 ; otherwise end signal
						cancelSound_case2:
							ldi temp, tone0
							sts currentTone, temp ; set sound tone  
						ret
						cancelSound_case3:
							cbr tempFlag2, sf_sound_enabled	; disable sound
							cbr tempFlag2, sf_cancel_signal_enabled ; end accept signal
							sts statesFlags0, tempFlag2
						ret



	updateWaveState:
		cbr eventsFlags0, ef_update_sound_wave_state

		ldi xh, high(currentTone)
		ldi xl, low(currentTone)
		ld temp, x

		out tcnt2, temp ; reset timer2 immediately to prevent bad sound

		ldi zh, high(statesFlags0)
		ldi zl, low(statesFlags0)
		ld tempFlag2, z
		sbrs tempFlag2, sf_sound_enabled_n ; if we should not prodice some sound
			reti ; do nothing
			sbis portd, speaker_pin_number ; if this pin is low
				jmp speakerWaveUp ; then make it high
				jmp speakerWaveDown ; otherwise make it low

			speakerWaveUp:
				sbi portd, speaker_pin_number ; make it high
				reti
			speakerWaveDown:
				cbi portd, speaker_pin_number ; make it low
				reti
				; result sound wave 				____	  ____	    ____	  ____
				; will be something like that:	___|	|____|	  |____|    |____|    |____


	readInput:
		in temp, eimsk ; disable external intrruption int0
		andi temp, 0b11111110
		out eimsk, temp

		cbr eventsFlags0, ef_handle_input
		
		ldi temp, 0b00000000 ; check first row
		out porta, temp
		in temp, pina ; get pressed button
		
		call checkInput

		ldi temp, 0b00001000 ; check 2nd row
		out porta, temp
		in temp, pina 

		call checkInput

		ldi temp, 0b00010000 ; check third row
		out porta, temp
		in temp, pina 

		call checkInput

		ldi temp, 0b00011000 ; check the last row
		out porta, temp
		in temp, pina ; 

		jmp endReadingInput

		checkInput:
			mov temp2, temp
			andi temp2, 0b00000111 ; check only three bits indicating pressed button
			cpi temp2, 0
			brne clearReturnAddr	
		ret

		clearReturnAddr:
			pop temp2 ; removing returning addr from stack
			pop temp2
		endReadingInput:
			sts currentKey, temp ; finally save pressed key
			ldi temp, 0b10000000 ; enable continuous keyboard checking
			out porta, temp 
			
			in temp, eimsk
			ldi temp2, 0b00000001
			or temp, temp2 ; enable external intrruption int0
			out eimsk, temp
	ret

	switchDisplayData:
		cbr eventsFlags1, ef_switch_display_data

		ldi zh, high(statesFlags0)
		ldi zl, low(statesFlags0)
		ld tempFlag2, z
		sbrc tempFlag2, sf_warning_signal_enabled_n
			jmp checkDisplayErrorMessage
			cbr tempFlag2, sf_display_error_message
			sts statesFlags0, tempFlag2
			jmp normalDataSwitch

		checkDisplayErrorMessage:
			sbrs tempFlag2, sf_display_error_message_n
				jmp switchThroughError
				jmp normalDataSwitch			

		switchThroughError:
			sbr tempFlag2, sf_display_error_message
			sts statesFlags0, tempFlag2
			jmp endSwitchingDisplayData

		normalDataSwitch:
			cbr tempFlag2, sf_display_error_message
			sts statesFlags0, tempFlag2
		ldi xh, high(currentDisplayData) ; get "number" of type of data we wanna to be displayed
				ldi xl, low(currentDisplayData)
				ld temp, x

				cpi temp, 1
					brsh goToFirstData
					;else next data
					inc temp
					sts currentDisplayData, temp
					jmp endSwitchingDisplayData

					goToFirstData:
						ldi temp, 0
						sts currentDisplayData, temp
		
		endSwitchingDisplayData:

	ret

	update7segScreen:
		cbr eventsFlags0, ef_update_7seg_screen

		ldi temp, 0xff 
		ldi temp2, 0xf0
		out tcnt1h, temp
		out tcnt1l, temp2
		
		ldi xh, high(curPos7Seg) ; get pereviously printed display indicator from ram
		ldi xl, low(curPos7Seg)
		ld temp3, x

		inc temp3

		cpi temp3, 8
			brsh jumpToFirst7seg
			jmp printGlyph

		jumpToFirst7seg:
			ldi temp3, 0 ; select first indicator
			call clear7SegScrBuffer
			ldi xh, high(currentDisplayData)
			ldi xl, low(currentDisplayData)
			ld temp, x
				
			ldi zh, high(statesFlags0)
			ldi zl, low(statesFlags0)
			ld tempFlag2, z
			sbrc tempFlag2, sf_display_error_message_n
				jmp selectErrorMessage
			cpi temp, 0
				breq selectTemperaturesData ; TODO prevent failing compairing after SBR command in interruption handler
			cpi temp, 1
				breq selectTachometerData

			selectTemperaturesData:
				call printEngineTemperature
				call printCurrentTemperature
				call printTargetTemperature
				jmp endJumpToFirstSegment

			selectTachometerData:
				call printTachometerValue
				jmp endJumpToFirstSegment

			selectErrorMessage:
				call printErrorMessage

			endJumpToFirstSegment:


		printGlyph:
			sts curPos7Seg, temp3

			; get number of a glyph 
			ldi xh, high(SevenSegScrBuff)
			ldi xl, low(SevenSegScrBuff)
			add  xl, temp3

			ld temp, x

			ldi zh,0
			ldi zl, charTable7seg * 2

			add zl, temp
			lpm temp, z

			cpi temp3, 1
				breq printDot
			cpi temp3, 4
				breq printDot
				jmp moveGlyphToscreen
			
			printDot:
				andi temp, 0b11111110

			moveGlyphToScreen:
			swap temp3
			lsl temp3
			out portb, temp3 ; select place on display

			out portc, temp ; print glyph
	ret

	clear7SegScrBuffer:
		ldi zh, 0
		ldi zl, charTable7seg * 2
		ldi temp, chSpace ; load space glyph
		;add zl, temp
		;lpm temp, z

		sts SevenSegScrBuff + 0, temp ; clear unused display indicators
		sts SevenSegScrBuff + 1, temp
		sts SevenSegScrBuff + 2, temp
		sts SevenSegScrBuff + 3, temp
		sts SevenSegScrBuff + 4, temp
		sts SevenSegScrBuff + 5, temp
		sts SevenSegScrBuff + 6, temp
		sts SevenSegScrBuff + 7, temp
	ret

	printTachometerValue:
		ldi xh, high(engineSpeed) ; get engineSpeed from ram
		ldi xl, low(engineSpeed)
		ld temp, x
	
		call convertBinToDec
		mov temp2, temp
		andi temp2, 0b00001111 ; select low page
		sts SevenSegScrBuff + 2 , temp2 ; place the actual glyph number in a buffer

		andi temp, 0b11110000 ; select high page
		swap temp
		sts SevenSegScrBuff + 1, temp ; place the actual glyph nubmer in a buffer

		ldi temp, ch0
		sts SevenSegScrBuff + 3, temp
		sts SevenSegScrBuff + 4, temp
	ret

	
	printTargetTemperature:
		ldi zh, high(statesFlags0)
		ldi zl, low(statesFlags0)
		ld tempFlag2, z
		sbrs tempFlag2, sf_conditioner_enabled_n ; if conditioner is not enabled then return emmidiately
			ret
		ldi xh, high(targetTemperature) ; get temperature from ram
		ldi xl, low(targetTemperature)
		ld temp, x
	
		call convertBinToDec
		mov temp2, temp
		andi temp2, 0b00001111 ; select low page
		sts SevenSegScrBuff + 4, temp2 ; place the actual glyph number in a buffer

		andi temp, 0b11110000 ; select high page
		swap temp
		sts SevenSegScrBuff + 3, temp ; place the actual glyph nubmer in a buffer
	ret

	printCurrentTemperature:
		ldi xh, high(analogValuesTable) ; get temperature from ram
		ldi xl, low(analogValuesTable)
		ld temp, x

		cpi temp, 128 ; check right at the beggining if this number negative
			brsh convertTemperature

		negateTemperature:
			ldi temp2,  chMinus ; "-" glyph number in a table
			sts SevenSegScrBuff + 5, temp2
			com temp
			
		convertTemperature:
			subi temp, 128
			call convertBinToDec
	
			mov temp2, temp
			andi temp2, 0b00001111 ; select low page
			sts SevenSegScrBuff + 7, temp2 ; place the actual glyph number in a buffer

			andi temp, 0b11110000 ; select high page
			swap temp
			sts SevenSegScrBuff + 6, temp ; place the actual glyph nubmer in a buffer
	ret

	printEngineTemperature:
		ldi xh, high(analogValuesTable) ; get temperature from ram
		ldi xl, low(analogValuesTable)
		ldi temp, 2
		add xl, temp
		ld temp, x
		
		cpi temp, 128
		 brlo printEngineTempNegative

		subi temp, 128
		call convertBinToDec
	
		mov temp2, temp
		andi temp2, 0b00001111 ; select low page
		sts SevenSegScrBuff + 1, temp2 ; place the actual glyph number in a buffer

		andi temp, 0b11110000 ; select high page
		swap temp
		sts SevenSegScrBuff, temp ; place the actual glyph nubmer in a buffer
		jmp endEngineTempPrinting

		printEngineTempNegative:
			ldi temp, chMinus ; load "minus" gliph number
			sts SevenSegScrBuff, temp
			sts SevenSegScrBuff + 1, temp

		endEngineTempPrinting:
	ret

	printErrorMessage:	
		ldi temp, chE
		sts SevenSegScrBuff, temp
		ldi temp, chR
		sts SevenSegScrBuff + 1, temp
		sts SevenSegScrBuff + 2, temp
		ldi temp, ch0
		sts sevenSegScrBuff + 3, temp
		ldi temp, chR
		sts sevenSegScrBuff + 4, temp
	ret

	convertBinToDec:
		cpi temp, 100 ; check right at the beggining if this number is to big to display it on the screen
		brsh btdMax
		ldi temp2, 0 ; yeeeeeee no looooopsss ahahahhachchahhhacha
			cpi temp, 10
			brge increaseTens0
				jmp setLowPage
		increaseTens0:
			subi temp, 10
			inc temp2
			cpi temp, 10
			brge increaseTens1
				jmp setLowPage
		increaseTens1:
			subi temp, 10
			inc temp2
			cpi temp, 10
			brge increaseTens2
				jmp setLowPage
		increaseTens2:
			subi temp, 10
			inc temp2
			cpi temp, 10
			brge increaseTens3
				jmp setLowPage
		increaseTens3:
			subi temp, 10
			inc temp2
			cpi temp, 10
			brge increaseTens4
				jmp setLowPage
		increaseTens4:
			subi temp, 10
			inc temp2
			cpi temp, 10
			brge increaseTens5
				jmp setLowPage
		increaseTens5:
			subi temp, 10
			inc temp2
			cpi temp, 10
			brge increaseTens6
				jmp setLowPage
		increaseTens6:
			subi temp, 10
			inc temp2
			cpi temp, 10
			brge increaseTens7
				jmp setLowPage
		increaseTens7:
			subi temp, 10
			inc temp2
			cpi temp, 10
			brge increaseTens8
				jmp setLowPage
		increaseTens8:
			subi temp, 10
			inc temp2
			cpi temp2, 10
				brge btdMax
				jmp setLowPage
		btdMax:
			ldi temp2, 9
			mov temp, temp2
			swap temp
			add temp, temp2
			
			jmp endBTDConv

		setLowPage:
			swap temp ; move low number to high page
			add temp, temp2 ; save hig number to low page
			swap temp

		endBTDConv:	
	ret


	readAnalogSensors:
		cbr eventsFlags0, ef_read_analog_sensors

		ldi xh, high(currentSensorNum) ; get the nubmer of thermal sensor we wanna read from
		ldi xl, low(currentSensorNum)
		ld temp2, x

		ldi zh, high(analogValuesTable) ; seting up addres of the beginning of temperatuers table
		ldi zl, low(analogValuesTable)

		add zl, temp2 ; apply offset to store data to correct place in a table

		in temp, adcl
		in temp, adch
		st z, temp ; and finally store it

		inc temp2 ; select next sensor value to be readen in the next iterration

		
		cpi temp2, 16 ; if next sensor is "non existing" 17th sensor then return to first one
			brge jumpToFirstSensor
			jmp saveNextSensorNumber ; else just save the next sensor nubmer

		jumpToFirstSensor:
			ldi temp2, 0

		saveNextSensorNumber:
			sts currentSensorNum, temp2
			lsl temp2
			in temp, pinf ; get content of portf
			andi temp, 0b11100001 ; clear 4 bits to rewrite them later
			or temp2, temp
			sts portf, temp2

		ldi temp, 0b01100000 ; and restart adc after all
		out admux, temp
		ldi temp, 0b11011110
		out adcsra, temp
	ret

	updateTachometer:
		cbr eventsFlags1, ef_update_tachometer
		
		ldi xh, high(tachometerCounter) ; get value counted by tachometer counter durring 0.1 sec
		ldi xl, low(tachometerCounter)
		ld temp, x
		;inc temp ; some kind of trikky compensation

		ldi temp2, 6
		mul temp, temp2 ; because it is 6*10 seconds in one minute
		mov temp, r0 ; get product low (numbers will not be to big)

		

		cpi temp, 96
			brsh tachometerMax
			jmp saveEngineSpeed

		tachometerMax:
			ldi temp, 99
		saveEngineSpeed:
			sts engineSpeed, temp
			ldi temp, 0
			sts tachometerCounter, temp
	ret

	incrementTachometerCounter:
		cbr eventsFlags1, ef_increment_tachometer

		in temp, eimsk ; disable external intrruption int1
		andi temp, 0b11111101
		out eimsk, temp

		ldi xh, high(tachometerCounter)
		ldi xl, low(tachometerCounter)
		ld temp, x
		
		cpi temp, 255
			breq endIncTachCounter
			jmp incTachCounter

		incTachCounter:
			inc temp
			sts tachometerCounter, temp
		
		endIncTachCounter:
			in temp, eimsk
			ldi temp2, 0b00000010
			or temp, temp2 ; enable external intrruption int1
			out eimsk, temp
	ret
	

	////////////INTERRUPTIONS HANDLERS/////////////
	extInt0Handler:
		in tempInter, sreg
		sbr eventsFlags0, ef_handle_input
		out sreg, tempInter
	reti

	extInt1Handler:
		in tempInter, sreg
		sbr eventsFlags1, ef_increment_tachometer
		out sreg, tempInter
	reti

	timer0OvfHandler:
		in tempInter, sreg
		sbr eventsFlags0, ef_update_sound_type
		out sreg, tempInter
	reti

	timer1OvfHandler:
		in tempInter, sreg
		sbr eventsFlags0, ef_update_7seg_screen
		out sreg, tempInter
	reti

	timer2OvfHandler: 
		in tempInter, sreg
		sbr eventsFlags0, ef_update_sound_wave_state
		out sreg, tempInter
	reti

	timer3OvfHandler:
		in tempInter, sreg
		sbr eventsFlags1, ef_update_program_timers
		out sreg, tempInter
	reti

	adcConvertionHandler:	
		in tempInter, sreg
		sbr eventsFlags0, ef_read_analog_sensors
		out sreg, tempInter
	reti