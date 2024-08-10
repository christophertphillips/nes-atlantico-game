.proc SpawnActors
  lda Clock60                           ; load timer value
  sec                                   ; subtract timer value from previous submarine timer value
  sbc PrevSubmarineClock
  cmp #3                                ; have more than 3 seconds passed since the last submarine was spawned?
  bne :+                                ; if no, skip
    SpawnSubmarine:                     ; else, spawn a submarine
      SET_ADD_ACTOR_ARGS #ActorType::SUBMARINE, #223, #185, #$FF, #$0
      jsr AddActor
      lda Clock60                       ; update submarine's timer value
      sta PrevSubmarineClock
  :

  rts
.endproc
