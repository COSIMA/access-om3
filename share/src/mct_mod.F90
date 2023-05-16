module mct_mod
  implicit none

contains

  subroutine mct_world_init(ncomps, globalcomm, mycomms, myids)
    integer, intent(in) :: ncomps
    integer, intent(in) :: globalcomm
    integer, dimension(:), pointer :: mycomms
    integer, dimension(:), pointer :: myids
  end subroutine mct_world_init

end module mct_mod
