.include "m128def.inc"
.def temp = r16 ; temporal registter for some actions
.def temp2 = r17
.def eventsFlags0 = r18
.def eventsFlags1 = r19
.def statesFlags0 = r20
.equ speaker_pin_number = 5
.equ speaker_pin_position = 0b00000100 ; speaker will be connected on 5th pin of portD
.equ heater_pin_number = 0
.equ heater_pin_position = 0b000000001
.equ cooler_pin_number = 1
.equ cooler_pin_position = 0b000000010
.equ tccr0_setup_byte = 0b00000101 ; select speed for timer0
.equ assr_setup_byte =  0b00001000 ; select async oscilator (slower speed and independent from main clock)
.equ tccr1b_setup_byte = 0b00000101 ; seelect speed for timer1
.equ tccr2_setup_byte = 0b00000101 ; select speed for timer2
.equ timsk_setup_byte = 0b01000101 ; enable timer2, tiemr1 and timer0 interruption on overflow
.def currentKey = r24
.equ soundSignalsDelay = 128 + 64 + 32 + 16 ; (lower number bigger delays)
	.equ tone0 = 0 + 128 + 64 + 32 + 16 + 8 + 4 ; (lower number -lower tone)
 	.equ tone1 = 0 + 128 + 64 + 32 + 16 + 8 + 4 + 2
.equ condFanSpeed = 249; higher number - higher speed
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
	.equ ef_update_sound_wave_state =	0b10000000
	.equ ef_update_sound_wave_state_n =	7
	.equ ef_update_sound_type =			0b01000000
	.equ ef_update_sound_type_n =		6
	.equ ef_handle_input =				0b00100000
	.equ ef_handle_input_n =			5
	.equ ef_update_7seg_screen =		0b00010000
	.equ ef_update_7seg_screen_n =		4
	.equ ef_read_temperatures =			0b00001000
	.equ ef_read_temperatures_n =		3
	.equ ef_save_target_temperature =	0b00000100
	.equ ef_save_target_temperature_n = 2
	.equ ef_update_heater =				0b00000010
	.equ ef_update_heater_n =			1
	.equ ef_update_conditioner_fan =	0b00000001
	.equ ef_update_conditioner_fan_n =	0
	.equ ef_load_target_temp =			0b10000000
	.equ ef_load_target_temp_n =		7
.equ targetTempSavingAddr = 0
.dseg
	currentTone:
		.byte 1 ; there will be stored current note of sound
	curPos7Seg:
		.byte 1 ; position of currently printing glyph on 7seg display
	SevenSegScrBuff:
		.byte 8 ; reserve 8 bytes for 7 segment dysplay buffer
	targetTemperature:
		.byte 1 ; reserve  1 byte to keep temperature while running\
	currentTemperature:
		.byte 1
	updateConditionerTimer:
		.byte 1
	conditionerFanTimer0:
		.byte 1 ; reserve 2 bytes to store current conditioner fan timer
	conditionerFanTimer1:
		.byte 1
	conditionerFanState:
		.byte 1 ; reserve 1 byte to know fan "rotation" (not quite but who cares)
.cseg
	.org 0
		jmp reset
		.org $0002
			jmp extInt0Handler
		.org $0014
			jmp timer2OvfHandler
		.org $001C
			jmp timer1OvfHandler
		.org $0020
			jmp timer0OvfHandler
		.org $002a
			jmp adcConvertionHandler
	.org $0050 ; start program almost right after all intrruptions vectors
	charTable7seg: 
		.db 0b00000011 /*0*/, 0b10011111 /*1*/, 0b00100101 /*2*/, 0b00001101 /*3*/
		.db	0b10011001 /*4*/, 0b01001001 /*5*/, 0b01000001 /*6*/, 0b00011111 /*7*/
		.db	0b00000001 /*8*/, 0b00001001 /*9*/, 0b00010001 /*a*/, 0b11000001 /*b*/
		.db	0b01100001 /*c*/, 0b10000101 /*d*/, 0b01100001 /*e*/, 0b01110001 /*f*/
		.db	0b00111011 /*grad*/, 0b11111101 /*minus*/, 0b01110011 /*r*/, 0xff /*space*/
	errorString7: 
		.db 14/*e*/, 18/*r*/, 18/*r*/,19/* */
	reset:
		ldi temp, high(ramend) ; setting up stack pointer
		out sph, temp
		ldi temp, low(ramend)
		out spl, temp
		ldi temp, 15
		call convertBinToDec
		ldi temp, 0b10011000 ; 0,1,2-in (read column), 3,4 - out (select row), 7 - out (check all buttons)
		out ddra, temp
		ldi temp, 0b10000000 ; enable continuous check if any button is pressed
		out porta, temp
		ldi temp, 0b11100000 ; set last hree bits of portB to be output
		out ddrb, temp
		ldi temp, 0b11111111 ; set all pins of portC to be output
		out ddrc, temp
		ldi temp, speaker_pin_position
		out ddrd, temp
		ldi temp, 0xff
		out ddre, temp
		ldi temp, 0b00000001 ; enable external intrruption int0
		out eimsk, temp
		ldi temp, 0b00000011 ; set ext interrupt0 on rising edge
		sts eicra, temp
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
		ldi temp, timsk_setup_byte
		out timsk, temp ; enable interupts on timers overflowing
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
		sbrc eventsFlags0, ef_read_temperatures_n
			call readTemperatures
		sbrc eventsFlags0, ef_save_target_temperature_n
			call saveTargetTemperature
		sbrc eventsFlags0, ef_update_heater_n
			call updateConditioner
		sbrc eventsFlags0, ef_update_conditioner_fan_n
			call updateConditionerFan
		sbrc eventsFlags1, ef_load_target_temp_n
			call loadTargetTemperature
		call updateProgramTimers ; update program timers on every background loop iteration
		cpi currentKey, 28
			breq kek
		cpi currentKey, 20
			breq lol
		cpi currentKey, 25
			breq cheburek
			jmp backgroundProcess
		lol:
			call lowerTemperature
			jmp backgroundProcess
		kek:
			call rizeTemperature
			jmp backgroundProcess
		cheburek:
			call switchConditioner
 	jmp backgroundProcess
	enableWarningSignal:
		sbr statesFlags0, sf_warning_signal_enabled
	ret
	enableClickSignal:
		sbr statesFlags0, sf_click_signal_enabled
	ret
	enableAcceptSignal:
		sbr statesFlags0, sf_accept_signal_enabled
	ret
	enableCancelSignal:
		sbr statesFlags0, sf_cancel_signal_enabled
	ret
	updateProgramTimers:
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
	ret
	rizeTemperature:
		ldi currentKey, 0
		ldi xh, high(targetTemperature) ; get temperature from ram
		ldi xl, low(targetTemperature)
		ld temp, x
		inc temp ; increment temperature
		cpi temp, 31 ; limit temperature to 30 grad
			brlo saveTemperatureToRam
			ldi temp, 30
		saveTemperatureToRam:
			sts targetTemperature, temp ;and save it to ram
		call convertBinToDec
		mov temp2, temp
		andi temp2, 0b00001111 ; select low page
		sts SevenSegScrBuff + 3, temp2 ; place the actual glyph number in a buffer
		andi temp, 0b11110000 ; select high page
		swap temp
		sts SevenSegScrBuff + 2, temp ; place the actual glyph nubmer in a buffer
		sbr eventsFlags0, ef_save_target_temperature
	ret
	lowerTemperature:
		ldi currentKey, 0
		ldi xh, high(targetTemperature) ; get temperature from ram
		ldi xl, low(targetTemperature)
		ld temp, x
		dec temp ; decrement temperature
		cpi temp, 10 ; limit temperature to 30 grad
			brge saveTemperatureToRam
			ldi temp, 10
		sts targetTemperature, temp ;and save it to ram
		call convertBinToDec
		mov temp2, temp
		andi temp2, 0b00001111 ; select low page
		sts SevenSegScrBuff + 3, temp2 ; place the actual glyph number in a buffer
		andi temp, 0b11110000 ; select high page
		swap temp
		sts SevenSegScrBuff + 2, temp ; place the actual glyph nubmer in a buffer
		sbr eventsFlags0, ef_save_target_temperature
	ret
	switchConditioner:
		sbrc eventsFlags0, sf_conditioner_enabled_n
			jmp cond_en
			jmp cond_dis
		cond_en:
			sbr statesFlags0, sf_conditioner_enabled
			jmp endSwitchConditioner
		cond_dis:
			cbr statesFlags0, sf_conditioner_enabled
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
		call convertBinToDec
		mov temp2, temp
		andi temp2, 0b00001111 ; select low page
		sts SevenSegScrBuff + 3, temp2 ; place the actual glyph number in a buffer
		andi temp, 0b11110000 ; select high page
		swap temp
		sts SevenSegScrBuff + 2, temp ; place the actual glyph nubmer in a buffer
		cbr eventsFlags1, ef_load_target_temp ; end loading task if it success
		endReadAttempt:
	ret
	updateConditioner:
		cbr eventsFlags0, ef_update_heater
		ldi xh, high(targetTemperature) ; get target temperature from ram
		ldi xl, low(targetTemperature)
		ld temp, x
		ldi xh, high(currentTemperature) ; get target temperature from ram
		ldi xl, low(currentTemperature)
		ld temp2, x
		cpi temp2, 128 ; if temperature is minus then enable heater emmediately 
		brlo enableHeater
		subi temp2, 128
		cp temp2, temp
		brlo enableHeater
		cbi porte, heater_pin_number
		sbi porte, cooler_pin_number
		jmp endUpdateConditioner
		enableHeater:
			sbi porte, heater_pin_number
			cbi porte, cooler_pin_number
		endUpdateConditioner: 
	ret
	updateConditionerFan:
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
	updateSoundType:
		cbr eventsFlags0, ef_update_sound_type
		ldi temp, soundSignalsDelay ; reset timer immediately
		out tcnt0, temp
		sbrc statesFlags0, sf_warning_signal_enabled_n
			jmp updateWarningSignal ; if warning signal is enabled then update it
		sbrc statesFlags0, sf_click_signal_enabled_n
			jmp updateClickSignal
		sbrc statesFlags0, sf_accept_signal_enabled_n
			jmp updateAcceptSignal
		sbrc statesFlags0, sf_cancel_signal_enabled_n
			jmp updateCancelSignal
		ret	
		updateWarningSignal:	
			sbrs statesFlags0, sf_sound_enabled_n
				jmp warningSound_case_0
				jmp warningSound_case_1
				warningSound_case_0:
					ldi temp, tone0
					sts currentTone, temp ; set sound tone
					sbr statesFlags0, sf_sound_enabled ; enable sound
				ret
				warningSound_case_1:
					cbr statesFlags0, sf_sound_enabled ; disable sound
				ret
		updateClickSignal:
			sbrs statesFlags0, sf_sound_enabled_n
				jmp clickSound_case_0 ; if we haven't clicked yet than click
				jmp clickSound_case_1 ; otherwise end lcick signal
				clickSound_case_0:
					ldi temp, tone0
					sts currentTone, temp ; set low sound tone  
					sbr statesFlags0, sf_sound_enabled ; enable sound
				ret
				clickSound_case_1:
					cbr statesFlags0, sf_click_signal_enabled ; end click sound
					cbr statesFlags0, sf_sound_enabled ; disable sound
				ret
		updateAcceptSignal:
			sbrs statesFlags0, sf_sound_enabled_n
				jmp acceptSound_case0 ; if sound is not enabled yet than enable it and set tone low
				jmp acceptSound_case1 ; otherwise if tone low then make it high. if tone high than end accept signal
				acceptSound_case0:	
					ldi temp, tone0		
					sts currentTone, temp ; set low sound tone  
					sbr statesFlags0, sf_sound_enabled ; enable sound
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
							cbr statesFlags0, sf_sound_enabled	; disable sound
							cbr statesFlags0, sf_accept_signal_enabled ; end accept signal
						ret
		updateCancelSignal:
			sbrs statesFlags0, sf_sound_enabled_n
				jmp cancelSound_case0 ; if sound is not enabled yet than enable it and set tone low
				jmp cancelSound_case1 ; otherwise if tone low then make it high. if tone high than end accept signal
				cancelSound_case0:
					ldi temp, tone1			
					sts currentTone, temp ; set high sound tone  
					sbr statesFlags0, sf_sound_enabled ; enable sound
				ret
				cancelSound_case1:
					ldi xh, high(conditionerFanTimer1)
					ldi xl, low(conditionerFanTimer1)
					ld temp, x
					cpi temp, tone1
						breq cancelSound_case2 ; if tone high make it low
						jmp cancelSound_case3 ; otherwise end signal
						cancelSound_case2:
							ldi temp, tone0
							sts currentTone, temp ; set sound tone  
						ret
						cancelSound_case3:
							cbr statesFlags0, sf_sound_enabled	; disable sound
							cbr statesFlags0, sf_cancel_signal_enabled ; end accept signal
						ret
	updateWaveState:
		cbr eventsFlags0, ef_update_sound_wave_state
		ldi xh, high(currentTone)
		ldi xl, low(currentTone)
		ld temp, x
		out tcnt2, temp ; reset timer2 immediately to prevent bad sound
		sbrs statesFlags0, sf_sound_enabled_n ; if we should not prodice some sound
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
	readInput:
		call enableClickSignal ; make click sound
		ldi temp, 0 ; disable external intrruption int0
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
			mov currentKey, temp ; finally save pressed key
			ldi temp, 0b10000000 ; enable continuous keyboard checking
			out porta, temp 
			ldi temp, 0b00000001 ; enable external intrruption int0
			out eimsk, temp
	ret
	update7segScreen:
		cbr eventsFlags0, ef_update_7seg_screen
		ldi zh,0
		ldi zl, charTable7seg * 2
		ldi temp, 1 ; load space glyph
		add zl, temp
		lpm temp, z
		sts SevenSegScrBuff + 0, temp ; clear unused display indicators
		sts SevenSegScrBuff + 1, temp
		sts SevenSegScrBuff + 4, temp
		ldi temp, 0xff 
		ldi temp2, 0xf0
		out tcnt1h, temp
		out tcnt1l, temp2
		ldi xh, high(curPos7Seg) ; get temperature from ram
		ldi xl, low(curPos7Seg)
		ld temp2, x
		inc temp2
		cpi temp2, 8
		breq jumpToFirst7seg
			jmp printGlyph
		jumpToFirst7seg:
			ldi temp2, 0 ; select first indicator
		printGlyph:
			sts curPos7Seg, temp2
			ldi temp, 255
			out portc, temp ; clear display place 
			ldi xh, high(SevenSegScrBuff)
			ldi xl, low(SevenSegScrBuff)
			add  xl, temp2
			ld temp, x
			ldi zh,0
			ldi zl, charTable7seg * 2
			add zl, temp
			lpm temp, z
			swap temp2
			lsl temp2
			out portb, temp2 ; select place on display
			out portc, temp ; print glyph
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
	readTemperatures:
		ldi temp2,  19 ; " " glyph number in a table
		sts SevenSegScrBuff + 5, temp2
		cbr eventsFlags0, ef_read_temperatures
		in temp, adcl
		in temp, adch
		sts currentTemperature, temp
		cpi temp, 128 ; check right at the beggining if this number is to big to display it on the screen
			brsh convertTemperature
		negateTemperature:
			ldi temp2,  17 ; "-" glyph number in a table
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
		ldi temp, 0b01100000 ; restart adc
		out admux, temp
		ldi temp, 0b11011110
		out adcsra, temp
	ret
	extInt0Handler:
		sbr eventsFlags0, ef_handle_input
	reti
	timer0OvfHandler:
		sbr eventsFlags0, ef_update_sound_type
	reti
	timer1OvfHandler:
		sbr eventsFlags0, ef_update_7seg_screen
	reti
	timer2OvfHandler: 
		sbr eventsFlags0, ef_update_sound_wave_state
	reti
	adcConvertionHandler:	
		sbr eventsFlags0, ef_read_temperatures
	reti