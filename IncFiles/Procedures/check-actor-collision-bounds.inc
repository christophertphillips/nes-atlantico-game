.proc CheckActorCollisionBounds

  lda CheckActorCollision_YPos
  cmp CheckActorCollisionBounds_Y1
  bcs End
      cmp CheckActorCollisionBounds_Y0
      bcc End
          lda CheckActorCollision_XPos
          cmp CheckActorCollisionBounds_X0
          bcc End
              cmp CheckActorCollisionBounds_X1
              bcs End
                  lda #$01
                  sta Collision

End:

  rts
.endproc
