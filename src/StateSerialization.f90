module StateSerialization

  use LevelsType
  use DomainType
  use ParametersType
  use WaterType
  use EnergyType
  use ForcingType
  use SnowWaterRenew
  use SnowLayerChange
  use messagepack
  use iso_fortran_env

  implicit none
  byte, dimension(:), allocatable :: serialization_buffer
  
contains

SUBROUTINE forcing_serialization (forcing)
    type(ForcingType), intent(in) :: forcing
    class(mp_arr_type), allocatable, intent(out) :: mp_arr
    mp_arr = mp_arr_type(22)
    mp_arr%values(1)%obj = mp_float_type(forcing%UU) !UU
    mp_arr%values(2)%obj = mp_float_type(forcing%VV) !VV
    mp_arr%values(3)%obj = mp_float_type(forcing%SFCTMP) !SFCTMP
    mp_arr%values(4)%obj = mp_float_type(forcing%Q2) !Q2
    mp_arr%values(5)%obj = mp_float_type(forcing%SFCPRS) !SFCPRS
    mp_arr%values(6)%obj = mp_float_type(forcing%SOLDN) !SOLDN
    mp_arr%values(7)%obj = mp_float_type(forcing%LWDN) !LWDN
    mp_arr%values(8)%obj = mp_int_type(forcing%YEARLEN) !YEARLEN, out
    mp_arr%values(9)%obj = mp_float_type(forcing%JULIAN) !JULIAN, out
    mp_arr%values(10)%obj = mp_float_type(forcing%THAIR) !THAIR, out
    mp_arr%values(11)%obj = mp_float_type(mforcing%QAIR) !QAIR, out
    mp_arr%values(12)%obj = mp_float_type(forcing%EAIR) !EAIR, out
    mp_arr%values(13)%obj = mp_float_type(forcing%RHOAIR) !RHOAIR, out
    mp_arr%values(14)%obj = mp_float_type(forcing%O2PP) !O2PP
    mp_arr%values(15)%obj = mp_float_type(forcing%CO2PP) !CO2PP
    mp_arr%values(16)%obj = mp_float_type(forcing%SWDOWN) !SWDOWN, out
    mp_arr%values(17)%obj = mp_float_type(forcing%SOLAD) !SOLAD
    mp_arr%values(18)%obj = mp_float_type(forcing%SOLAI) !SOLAI
    mp_arr%values(19)%obj = mp_float_type(forcing%PRCP) !PRCP
    mp_arr%values(20)%obj = mp_float_type(forcing%PRCPNONC) !PRCPNONC
    mp_arr%values(21)%obj = mp_float_type(mforcing%FPICE) !FPICE, out 
    mp_arr%values(22)%obj = mp_float_type(forcing%UR) !UR, out

END SUBROUTINE forcing_serialization

SUBROUTINE domain_serialization (domain)
    type(ForcingType), intent(in) :: domain
    class(mp_arr_type), allocatable, intent(out) :: mp_arr

    mp_arr = mp_arr_type(6)
    mp_arr%values(1)%obj = mp_float_type(domain%curr_datetime) !curr_datetime
    mp_arr%values(2)%obj = mp_float_type(domain%ITIME) !ITIME
    mp_arr%values(3)%obj = mp_float_type(domain%time_dbl) !time_dbl
    mp_arr%values(4)%obj = mp_float_type(domain%nowdate) !nowdate

    mp_arr%values(5)%obj = mp_arr_type(domain%DZSNSO) !DZSNSO have to fix indices using levels%soil
    mp_arr%values(6)%obj = mp_arr_type(domain%ZSNSO) !ZSNSO have to fix indices using levels%soil

END SUBROUTINE domain_serialization (domain)

SUBROUTINE domain_serialization (domain)
    type(ForcingType), intent(in) :: domain
    class(mp_arr_type), allocatable, intent(out) :: mp_arr

    mp_arr = mp_arr_type(6)
    mp_arr%values(1)%obj = mp_float_type(domain%curr_datetime) !curr_datetime
    mp_arr%values(2)%obj = mp_float_type(domain%ITIME) !ITIME
    mp_arr%values(3)%obj = mp_float_type(domain%time_dbl) !time_dbl
    mp_arr%values(4)%obj = mp_float_type(domain%nowdate) !nowdate

    mp_arr%values(5)%obj = mp_arr_type(domain%DZSNSO) !DZSNSO have to fix indices using levels%soil
    mp_arr%values(6)%obj = mp_arr_type(domain%ZSNSO) !ZSNSO have to fix indices using levels%soil

END SUBROUTINE domain_serialization (domain)

END Module