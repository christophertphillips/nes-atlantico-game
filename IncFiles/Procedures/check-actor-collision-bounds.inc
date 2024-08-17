.proc CheckActorCollisionBounds
CheckY1Bounds:
  lda CheckActorCollision_YPos                    ; load YPos of source actor (Y_Source)
  cmp CheckActorCollisionBounds_Y1                ; is Y_Source > Y1_Target?
  bcs EndCollisionBoundsCheck                     ; if yes, end check; else, proceed

    CheckY0Bounds:
      cmp CheckActorCollisionBounds_Y0            ; is Y_Source < Y0_Target?
      bcc EndCollisionBoundsCheck                 ; if yes, end check; else, proceed

        CheckX0Bounds:
          lda CheckActorCollision_XPos            ; load XPos of source actor (X_Source)
          cmp CheckActorCollisionBounds_X0        ; is X_Source < X0_Target?
          bcc EndCollisionBoundsCheck             ; if yes, end check; else, proceed

            CheckX1Bounds:
              cmp CheckActorCollisionBounds_X1    ; is X_Source > X1_Target?
              bcs EndCollisionBoundsCheck         ; if yes, end check; else, proceed
                  lda #$01                        ; store 1 in collision to indicate collision occurred
                  sta Collision

EndCollisionBoundsCheck:
  rts
.endproc
