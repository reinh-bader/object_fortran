module timer
! Timer implementation based on system_clock intrinsic
! Note that this facility is not thread-safe
  implicit none
  private
  public :: wtime
  integer, parameter :: ik = selected_int_kind(12)
  logical, save :: first = .true.
  integer(ik), save ::  count_rate, count_max
  double precision, save :: conversion = 0.0d0
contains
  double precision function wtime()
    integer(ik) :: count
    if (first) then
       first = .false.
       call system_clock(count, count_rate, count_max)
       conversion = 1.0d0 / dble(count_rate)
    else
       call system_clock(count)
    end if
    wtime = count * conversion
  end function wtime
end module timer
