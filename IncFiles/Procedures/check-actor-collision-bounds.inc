.proc CheckActorCollisionBounds
CheckY1Bounds:
  lda CheckActorCollision_YPos
  cmp CheckActorCollisionBounds_Y1
  bcs EndCollisionBoundsCheck

    CheckY0Bounds:
      cmp CheckActorCollisionBounds_Y0
      bcc EndCollisionBoundsCheck

        CheckX0Bounds:
          lda CheckActorCollision_XPos
          cmp CheckActorCollisionBounds_X0
          bcc EndCollisionBoundsCheck

            CheckX1Bounds:
              cmp CheckActorCollisionBounds_X1
              bcs EndCollisionBoundsCheck
                  lda #$01
                  sta Collision

EndCollisionBoundsCheck:
  rts
.endproc
