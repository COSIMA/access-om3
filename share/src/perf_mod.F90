module perf_mod
  use shr_kind_mod
  use ESMF
  implicit none

contains

  subroutine t_setLogUnit(LogUnit)
    integer(SHR_KIND_IN), intent(IN) :: LogUnit  ! Unit number for log output

  end subroutine t_setLogUnit

  subroutine t_prf(filename, mpicom, num_outpe, stride_outpe, single_file, global_stats, output_thispe)
    character(len=*), intent(in), optional :: filename
    integer, intent(in), optional :: mpicom
    integer, intent(in), optional :: num_outpe
    integer, intent(in), optional :: stride_outpe
    logical, intent(in), optional :: single_file
    logical, intent(in), optional :: global_stats
    logical, intent(in), optional :: output_thispe

  end subroutine t_prf

  subroutine t_initf(NLFilename, LogPrint, LogUnit, mpicom, MasterTask, MaxThreads)
    character(len=*),   intent(IN) :: NLFilename
    logical, optional,  intent(IN) :: LogPrint
    integer, optional,  intent(IN) :: LogUnit
    integer, optional,  intent(IN) :: mpicom
    logical, optional,  intent(IN) :: MasterTask
    integer, optional,  intent(IN) :: MaxThreads

  end subroutine t_initf

  subroutine t_finalizef()

  end subroutine t_finalizef

  subroutine t_startf(event, handle)
    character(len=*), intent(in) :: event
    integer,  optional :: handle
    call ESMF_TraceRegionEnter(event)
  end subroutine t_startf

  subroutine t_stopf(event, handle)
    character(len=*), intent(in) :: event
    integer,  optional :: handle
    call ESMF_TraceRegionExit(event)
  end subroutine t_stopf

  subroutine t_barrierf(event, mpicom)
    integer, intent(in), optional :: mpicom
    character(len=*), intent(in), optional :: event
  end subroutine t_barrierf

end module perf_mod

! The following functions are called from shr_mem_mod without an interface. We
! add some stubs here.
integer function GPTLget_memusage(size, rss, share, text, datastack)
  integer :: size, rss, share, text, datastack
  GPTLget_memusage = 0
end function GPTLget_memusage

integer function GPTLprint_memusage(str)
  character(len=*) :: str
  GPTLprint_memusage = 0
end function GPTLprint_memusage
