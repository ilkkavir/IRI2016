       subroutine init()
c       COMMON/const2/icalls,nmono,iyearo,idaynro,rzino,igino,ut0
c
c       icalls=0
c       nmono=-1
c       iyearo=-1
c       idaynro=-1
c       rzino=-1
c       igino=-1
c       ut0=-1

       call read_ig_rz
       call readapf107

       end subroutine init
