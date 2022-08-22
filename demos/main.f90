MODULE mod_QAOutput

    implicit none
    character(len=4) :: name
    integer(singI)   :: id
  !  
  !   id=1     node
  !   id=2     beam element
  !   id=3     solid element
  !   id=4     shell element
  !   id=5     thick shell element
  !   id=6     discrete element
  !
     type_id: select case (name)
        case ('node')
          id=1
        case ('beam')
          id=2
        case ('soli')
          id=3
        case ('shel')
          id=4
        case ('t_sh')
          id=5
        case ('disc')
          id=6
     end select type_id
END MODULE mod_QAOutput