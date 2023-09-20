module m_MCTWorld
  implicit none

contains

  subroutine init(ncomps, globalcomm, mycomms, myids)
    integer, intent(in) :: ncomps
    integer, intent(in) :: globalcomm
    integer, dimension(:), pointer :: mycomms
    integer, dimension(:), pointer :: myids
  end subroutine init

end module m_MCTWorld
