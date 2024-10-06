.proc SpawnActors
  lda Clock60                           ; load timer value
  sec                                   ; subtract timer value from previous submarine timer value
  sbc PrevSubmarineClock
  cmp #3                                ; have more than 3 seconds passed since the last submarine was spawned?
  bne :++                                ; if no, skip
    SpawnSubmarine:                     ; else, spawn a submarine
      SET_ADD_ACTOR_ARGS #ActorType::SUBMARINE, #$FF, #$B9, #$FF, #$0

      jsr GetRandomByte                 ; get a random byte

    SubmarineClampLoop:                 ; clamp range of subs in [184, 224]
      cmp #40                           ; perform (modulo 40)
      bcc :+
          sec
          sbc #40
          jmp SubmarineClampLoop
      :
      clc                               ; shift final position 184 pixels down
      adc #184

      sta AddActor_YPos                 ; overwrite YPos arg with randomized value
      jsr AddActor
      lda Clock60                       ; update submarine's timer value
      sta PrevSubmarineClock
  :

  lda Clock60                           ; load timer value
  sec                                   ; subtract timer value from previous airplane timer value
  sbc PrevAirplaneClock
  cmp #2                                ; have more than 3 seconds passed since the last airplane was spawned?
  bne :++                               ; if no, skip
    SpawnAirplane:                      ; else, spawn a airplane
      SET_ADD_ACTOR_ARGS #ActorType::AIRPLANE, #232, #$30, #$FE, #$0

      jsr GetRandomByte                 ; get a random byte

    AirplaneClampLoop:                  ; clamp range of airplanes in [184, 224]
      cmp #88                           ; perform (modulo 88)
      bcc :+
          sec
          sbc #88
          jmp AirplaneClampLoop
      :
      clc                               ; shift final position 40 pixels down
      adc #40

      sta AddActor_YPos                 ; overwrite YPos arg with randomized value
      jsr AddActor
      lda Clock60                       ; update airplane's timer value
      sta PrevAirplaneClock
  :

  rts
.endproc
