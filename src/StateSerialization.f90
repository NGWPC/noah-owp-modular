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
    
contains

SUBROUTINE forcing_serialization (forcing, mp_arr)
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
    mp_arr%values(17)%obj = mp_arr_type(forcing%SOLAD(1:2)) !SOLAD
    mp_arr%values(18)%obj = mp_arr_type(forcing%SOLAI(1:2)) !SOLAI
    mp_arr%values(19)%obj = mp_float_type(forcing%PRCP) !PRCP
    mp_arr%values(20)%obj = mp_float_type(forcing%PRCPNONC) !PRCPNONC
    mp_arr%values(21)%obj = mp_float_type(mforcing%FPICE) !FPICE, out 
    mp_arr%values(22)%obj = mp_float_type(forcing%UR) !UR, out

END SUBROUTINE forcing_serialization

SUBROUTINE forcing_deserialization (mp_arr, forcing)
    class(mp_arr_type), allocatable, intent(in) :: mp_arr
    type(ForcingType), intent(inout) :: forcing
    real(kind=real64) :: deserialized_val
    class(mp_arr_type), allocatable :: mp_sub_arr
    logical :: status
    integer(kind=int64) :: index, sub_index 

    do index=1, size(mp_arr)
        if (index = 8) then
            call get_int(arr%values(index)%obj, deserialized_val, status)
        else if (index = 17 .OR. index = 18) then
            if (is_arr(arr%values(index)%obj)) then
                mp_sub_arr = arr%values(index)%obj
            else
                !write to log file
            end if
        else    
            call get_real(arr%values(index)%obj, deserialized_val, status)
        end if
        select case(index)
            case(1)
                forcing%UU = deserialized_val
            case(2)
                forcing%VV = deserialized_val
            case(3)
                forcing%SFCTMP = deserialized_val
            case(4)
                forcing%Q2 = deserialized_val
            case(5)
                forcing%SFCPRS = deserialized_val
            case(6)
                forcing%SOLDN = deserialized_val
            case(7)
                forcing%LWDN = deserialized_val
            case(8)
                forcing%YEARLEN = deserialized_val
            case(9)
                forcing%JULIAN = deserialized_val
            case(10)
                forcing%THAIR = deserialized_val
            case(11)
                mforcing%QAIR = deserialized_val
            case(12)
                forcing%EAIR = deserialized_val
            case(13)
                forcing%RHOAIR = deserialized_val
            case(14)
                forcing%O2PP = deserialized_val
            case(15)
                forcing%CO2PP = deserialized_val
            case(16)
                forcing%SWDOWN = deserialized_val
            case(17)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    forcing%SOLAD(sub_index) = deserialized_val
                end do
            case(18)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    forcing%SOLAI(sub_index) = deserialized_val
                end do
            case(19)
                forcing%PRCP = deserialized_val
            case(20)
                forcing%PRCPNONC = deserialized_val
            case(21)
                forcing%FPICE = deserialized_val
            case(22)
                forcing%UR = deserialized_val
        end select
    end do
END SUBROUTINE forcing_deserialization


SUBROUTINE domain_serialization (domain, mp_arr)
    type(DomainType), intent(in) :: domain
    class(mp_arr_type), allocatable, intent(out) :: mp_arr

    mp_arr = mp_arr_type(6)
    mp_arr%values(1)%obj = mp_float_type(domain%curr_datetime) !curr_datetime
    mp_arr%values(2)%obj = mp_float_type(domain%ITIME) !ITIME
    mp_arr%values(3)%obj = mp_float_type(domain%time_dbl) !time_dbl
    mp_arr%values(4)%obj = mp_float_type(domain%nowdate) !nowdate

    mp_arr%values(5)%obj = mp_arr_type(domain%DZSNSO)
    mp_arr%values(6)%obj = mp_arr_type(domain%ZSNSO)

END SUBROUTINE domain_serialization 

SUBROUTINE domain_deserialization (mp_arr, domain)
    class(mp_arr_type), allocatable, intent(in) :: mp_arr
    type(DomainType), intent(inout) :: domain
    real(kind=real64) :: deserialized_val
    class(mp_arr_type), allocatable :: mp_sub_arr
    logical :: status
    integer(kind=int64) :: index, sub_index 

    do index=1, size(mp_arr)
        if (index = 5 .OR. index = 6) then
            if (is_arr(arr%values(index)%obj)) then
                mp_sub_arr = arr%values(index)%obj
            else
                !write to log file
            end if
        else    
            call get_real(arr%values(index)%obj, deserialized_val, status)
        end if
        select case(index)
            case(1)
                domain%curr_datetime = deserialized_val
            case(2)
                domain%ITIME = deserialized_val
            case(3)
                domain%time_dbl = deserialized_val
            case(4)
                domain%nowdate = deserialized_val
            case(5)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    domain%DZSNSO(sub_index) = deserialized_val
                end do
            case(6)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    domain%ZSNSO(sub_index) = deserialized_val
                end do
        end select
    end do
END SUBROUTINE domain_deserialization

SUBROUTINE energy_serialization (energy, mp_arr)
    type(EnergyType), intent(in) :: energy
    class(mp_arr_type), allocatable, intent(out) :: mp_arr

    mp_arr = mp_arr_type(125)
    mp_arr%values(1)%obj = mp_float_type(energy%cosz) !cosz
    mp_arr%values(2)%obj = mp_float_type(energy%cosz_horiz) !cosz_horiz
    mp_arr%values(3)%obj = mp_float_type(energy%TAH) !TAH
    mp_arr%values(4)%obj = mp_float_type(energy%EAH) !EAH
    mp_arr%values(5)%obj = mp_float_type(energy%IGS) !IGS
    mp_arr%values(6)%obj = mp_float_type(energy%TAUXV) !TAUXV
    mp_arr%values(7)%obj = mp_float_type(energy%TAUYV) !TAUYV
    mp_arr%values(8)%obj = mp_float_type(energy%IRC) !IRC
    mp_arr%values(9)%obj = mp_float_type(energy%SHC) !SHC
    mp_arr%values(10)%obj = mp_float_type(energy%IRG) !IRG
    mp_arr%values(11)%obj = mp_float_type(energy%SHG) !SHG
    mp_arr%values(12)%obj = mp_float_type(energy%EVG) !EVG
    mp_arr%values(13)%obj = mp_float_type(energy%EVC) !EVC
    mp_arr%values(14)%obj = mp_float_type(energy%TR) !TR
    mp_arr%values(15)%obj = mp_float_type(energy%GHV) !GHV
    mp_arr%values(16)%obj = mp_float_type(energy%PSNSUN) !PSNSUN
    mp_arr%values(17)%obj = mp_float_type(energy%PSNSHA) !PSNSHA
    mp_arr%values(18)%obj = mp_float_type(energy%T2MV) !T2MV
    mp_arr%values(19)%obj = mp_float_type(energy%Q2V) !Q2V
    mp_arr%values(20)%obj = mp_float_type(energy%CHV) !CHV
    mp_arr%values(21)%obj = mp_float_type(energy%CHLEAF) !CHLEAF
    mp_arr%values(22)%obj = mp_float_type(energy%CHUC) !CHUC
    mp_arr%values(23)%obj = mp_float_type(energy%CHV2) !CHV2
    mp_arr%values(24)%obj = mp_float_type(energy%RB) !RB
    mp_arr%values(25)%obj = mp_float_type(energy%Z0MG) !Z0MG
    mp_arr%values(26)%obj = mp_float_type(energy%Z0M) !Z0M
    mp_arr%values(27)%obj = mp_float_type(energy%ZPD) !ZPD
    mp_arr%values(28)%obj = mp_float_type(energy%ZLVL) !ZLVL
    mp_arr%values(29)%obj = mp_float_type(energy%EMG) !EMG
    mp_arr%values(30)%obj = mp_float_type(energy%RSURF) !RSURF
    mp_arr%values(31)%obj = mp_float_type(energy%RHSUR) !RHSUR
    mp_arr%values(32)%obj = mp_float_type(energy%LATHEAV) !LATHEAV
    mp_arr%values(33)%obj = mp_float_type(energy%frozen_canopy) !frozen_canopy
    mp_arr%values(34)%obj = mp_float_type(energy%GAMMAV) !GAMMAV
    mp_arr%values(35)%obj = mp_float_type(energy%LATHEAG) !LATHEAG
    mp_arr%values(36)%obj = mp_float_type(energy%frozen_ground) !frozen_ground
    mp_arr%values(37)%obj = mp_float_type(energy%GAMMAG) !GAMMAG
    mp_arr%values(38)%obj = mp_float_type(energy%TGB) !TGB
    mp_arr%values(39)%obj = mp_float_type(energy%CMB) !CMB
    mp_arr%values(40)%obj = mp_float_type(energy%CHB) !CHB
    mp_arr%values(41)%obj = mp_float_type(energy%Z0WRF) !Z0WRF
    mp_arr%values(42)%obj = mp_float_type(energy%RSSUN) !RSSUN
    mp_arr%values(43)%obj = mp_float_type(energy%T2M) !T2M
    mp_arr%values(44)%obj = mp_float_type(energy%Q1) !Q1
    mp_arr%values(45)%obj = mp_float_type(energy%Q2E) !Q2E
    mp_arr%values(46)%obj = mp_float_type(energy%FGEV) !FGEV
    mp_arr%values(47)%obj = mp_float_type(energy%TS) !TS
    mp_arr%values(48)%obj = mp_float_type(energy%TAUY) !TAUY
    mp_arr%values(49)%obj = mp_float_type(energy%GH) !GH
    mp_arr%values(50)%obj = mp_float_type(energy%SSOIL) !SSOIL
    mp_arr%values(51)%obj = mp_float_type(energy%TGV) !TGV
    mp_arr%values(52)%obj = mp_float_type(energy%FCEV) !FCEV
    mp_arr%values(53)%obj = mp_float_type(energy%CM) !CM
    mp_arr%values(54)%obj = mp_float_type(energy%FIRA) !FIRA
    mp_arr%values(55)%obj = mp_float_type(energy%RSSHA) !RSSHA
    mp_arr%values(56)%obj = mp_float_type(energy%TG) !TG
    mp_arr%values(57)%obj = mp_float_type(energy%CH) !CH
    mp_arr%values(58)%obj = mp_float_type(energy%FCTR) !FCTR
    mp_arr%values(59)%obj = mp_float_type(energy%PAH) !PAH
    mp_arr%values(60)%obj = mp_float_type(energy%TAUX) !TAUX
    mp_arr%values(61)%obj = mp_float_type(energy%FSH) !FSH
    mp_arr%values(62)%obj = mp_float_type(energy%EMISSI) !EMISSI
    mp_arr%values(63)%obj = mp_float_type(energy%TRAD) !TRAD
    mp_arr%values(64)%obj = mp_float_type(energy%APAR) !APAR
    mp_arr%values(65)%obj = mp_float_type(energy%PSN) !PSN
    mp_arr%values(66)%obj = mp_float_type(energy%STC) !STC
    mp_arr%values(67)%obj = mp_float_type(energy%LH) !LH
    mp_arr%values(68)%obj = mp_float_type(energy%TGS) !TGS
    mp_arr%values(69)%obj = mp_arr_type(energy%HCPCT ) !HCPCT array  (-levels%NSNOW+1:levels%NSOIL)
    mp_arr%values(70)%obj = mp_arr_type(energy%DF ) !DF array  (-levels%NSNOW+1:levels%NSOIL)
    mp_arr%values(71)%obj = mp_arr_type(energy%FACT ) !FACT array  (-levels%NSNOW+1:levels%NSOIL)
    mp_arr%values(72)%obj = mp_float_type(energy%PAHV) !PAHV
    mp_arr%values(73)%obj = mp_float_type(energy%PAHG) !PAHG
    mp_arr%values(74)%obj = mp_float_type(energy%PAHB) !PAHB
    mp_arr%values(75)%obj = mp_float_type(energy%FSHA) !FSHA
    mp_arr%values(76)%obj = mp_float_type(energy%LAISUN) !LAISUN
    mp_arr%values(77)%obj = mp_float_type(energy%LAISHA) !LAISHA
    mp_arr%values(78)%obj = mp_float_type(energy%BGAP) !BGAP
    mp_arr%values(79)%obj = mp_float_type(energy%WGAP) !WGAP
    mp_arr%values(80)%obj = mp_arr_type(energy%ALBD)  !ALBD array  (1:parameters%NBAND)
    mp_arr%values(81)%obj = mp_arr_type(energy%ALBI) !ALBI array  (1:parameters%NBAND)
    mp_arr%values(82)%obj = mp_arr_type(energy%ALBGRD) !ALBGRD array  (1:parameters%NBAND)
    mp_arr%values(83)%obj = mp_arr_type(energy%ALBGRI) !ALBGRI array  (1:parameters%NBAND)
    mp_arr%values(84)%obj = mp_arr_type(energy%ALBSND) !ALBSND array  (1:parameters%NBAND)
    mp_arr%values(85)%obj = mp_arr_type(energy%ALBSNI) !ALBSNI array  (1:parameters%NBAND)
    mp_arr%values(86)%obj = mp_arr_type(energy%FABD) !FABD array  (1:parameters%NBAND)
    mp_arr%values(87)%obj = mp_arr_type(energy%FABI) !FABI array  (1:parameters%NBAND)
    mp_arr%values(88)%obj = mp_arr_type(energy%FTDD) !FTDD array  (1:parameters%NBAND)
    mp_arr%values(89)%obj = mp_arr_type(energy%FTID) !FTID array  (1:parameters%NBAND)
    mp_arr%values(90)%obj = mp_arr_type(energy%FTII) !FTII array  (1:parameters%NBAND)
    mp_arr%values(91)%obj = mp_arr_type(energy%RHO) !RHO array  (1:parameters%NBAND)
    mp_arr%values(92)%obj = mp_arr_type(energy%TAU) !TAU array  (1:parameters%NBAND)
    mp_arr%values(93)%obj = mp_float_type(energy%FSUN) !FSUN
    mp_arr%values(94)%obj = mp_float_type(energy%TAUSS) !TAUSS
    mp_arr%values(95)%obj = mp_float_type(energy%FAGE) !FAGE
    mp_arr%values(96)%obj = mp_float_type(energy%ALB) !ALB
    mp_arr%values(97)%obj = mp_float_type(energy%ALBOLD) !ALBOLD
    mp_arr%values(98)%obj = mp_arr_type(energy%FTDI) !FTDI array  (1:2)
    mp_arr%values(99)%obj = mp_arr_type(energy%FREVD) !FREVD array  (1:2)
    mp_arr%values(100)%obj = mp_arr_type(energy%FREGD) !FREGD array  (1:2)
    mp_arr%values(101)%obj = mp_arr_type(energy%FREVI) !FREVI array  (1:2)
    mp_arr%values(102)%obj = mp_arr_type(energy%FREGI) !FREGI array  (1:2)
    mp_arr%values(103)%obj = mp_float_type(energy%SAG) !SAG
    mp_arr%values(104)%obj = mp_float_type(energy%SAV) !SAV
    mp_arr%values(105)%obj = mp_float_type(energy%FSA) !FSA
    mp_arr%values(106)%obj = mp_float_type(energy%PARSUN) !PARSUN
    mp_arr%values(107)%obj = mp_float_type(energy%PARSHA) !PARSHA
    mp_arr%values(108)%obj = mp_float_type(energy%FSR) !FSR
    mp_arr%values(109)%obj = mp_float_type(energy%FSRV) !FSRV
    mp_arr%values(110)%obj = mp_float_type(energy%FSRG) !FSRG
    mp_arr%values(111)%obj = mp_float_type(energy%QSFC) !QSFC
    mp_arr%values(112)%obj = mp_float_type(energy%TV) !TV
    mp_arr%values(113)%obj = mp_float_type(energy%CAH2) !CAH2
    mp_arr%values(114)%obj = mp_float_type(energy%IRB) !IRB
    mp_arr%values(115)%obj = mp_float_type(energy%SHB) !SHB
    mp_arr%values(116)%obj = mp_float_type(energy%EVB) !EVB
    mp_arr%values(117)%obj = mp_float_type(energy%GHB) !GHB
    mp_arr%values(118)%obj = mp_float_type(energy%TAUXB) !TAUXB
    mp_arr%values(119)%obj = mp_float_type(energy%TAUYB) !TAUYB
    mp_arr%values(120)%obj = mp_float_type(energy%EHB2) !EHB2
    mp_arr%values(121)%obj = mp_float_type(energy%T2MB) !T2MB
    mp_arr%values(122)%obj = mp_float_type(energy%Q2B) !Q2B
    mp_arr%values(123)%obj = mp_arr_type(energy%IMELT) !IMELT array  (-levels%NSNOW+1:levels%NSOIL)
    mp_arr%values(124)%obj = mp_float_type(energy%QMELT) !QMELT
    mp_arr%values(125)%obj = mp_arr_type(energy%SNOWT_AVG) !SNOWT_AVG array,  could be realMissing

END SUBROUTINE energy_serialization

SUBROUTINE energy_deserialization (mp_arr, energy)
    class(mp_arr_type), allocatable, intent(in) :: mp_arr
    type(EnergyType), intent(inout) :: energy
    real(kind=real64) :: deserialized_val
    class(mp_arr_type), allocatable :: mp_sub_arr
    logical :: status
    integer(kind=int64) :: index, sub_index 

    do index=1, size(mp_arr)
        if ((index >= 69 .AND. index <= 71) .OR. (index >= 80 .AND. index <= 92) .OR. (index >= 98 .AND. index <= 102) .OR. (index = 123) .OR. (index = 125)) then
            if (is_arr(arr%values(index)%obj)) then
                mp_sub_arr = arr%values(index)%obj
            else
                !write to log file
            end if
        else    
            call get_real(arr%values(index)%obj, deserialized_val, status)
        end if
        select case(index)
            case(1)
                energy%cosz = deserialized_val
            case(2)
                energy%cosz_horiz = deserialized_val
            case(3)
                energy%TAH = deserialized_val
            case(4)
                energy%EAH = deserialized_val
            case(5)
                energy%IGS = deserialized_val
            case(6)
                energy%TAUXV = deserialized_val
            case(7)
                energy%TAUYV = deserialized_val
            case(8)
                energy%IRC = deserialized_val
            case(9)
                energy%SHC = deserialized_val
            case(10)
                energy%IRG = deserialized_val
            case(11)
                energy%SHG = deserialized_val
            case(12)
                energy%EVG = deserialized_val
            case(13)
                energy%EVC = deserialized_val
            case(14)
                energy%TR = deserialized_val
            case(15)
                energy%PSNSUN = deserialized_val
            case(17)
                energy%PSNSHA = deserialized_val
            case(18)
                energy%T2MV = deserialized_val
            case(19)
                energy%Q2V = deserialized_val
            case(20)
                energy%CHV = deserialized_val
            case(21)
                energy%CHLEAF = deserialized_val
            case(22)
                energy%CHUC = deserialized_val
            case(23)
                energy%CHV2 = deserialized_val
            case(24)
                energy%RB = deserialized_val
            case(25)
                energy%Z0MG = deserialized_val
            case(26)
                energy%Z0M = deserialized_val
            case(27)
                energy%ZPD = deserialized_val
            case(28)
                energy%ZLVL = deserialized_val
            case(29)
                energy%EMG = deserialized_val
            case(30)
                energy%RSURF = deserialized_val
            case(31)
                energy%RHSUR = deserialized_val
            case(32)
                energy%LATHEAV = deserialized_val
            case(33)
                energy%frozen_canopy = deserialized_val
            case(34)
                energy%GAMMAV = deserialized_val
            case(35)
                energy%LATHEAG = deserialized_val
            case(36)
                energy%frozen_ground = deserialized_val
            case(37)
                energy%GAMMAG = deserialized_val
            case(38)
                energy%TGB = deserialized_val
            case(39)
                energy%CMB = deserialized_val
            case(40)
                energy%CHB = deserialized_val
            case(41)
                energy%Z0WRF = deserialized_val
            case(42)
                energy%RSSUN = deserialized_val
            case(43)
                energy%T2M = deserialized_val
            case(44)
                energy%Q1 = deserialized_val
            case(45)
                energy%Q2E = deserialized_val
            case(46)
                energy%FGEV = deserialized_val
            case(47)
                energy%TS = deserialized_val
            case(48)
                energy%TAUY = deserialized_val
            case(49)
                energy%GH = deserialized_val
            case(50)
                energy%SSOIL = deserialized_val
            case(51)
                energy%TGV = deserialized_val
            case(52)
                energy%FCEV = deserialized_val
            case(53)
                energy%CM = deserialized_val
            case(54)
                energy%FIRA = deserialized_val
            case(55)
                energy%RSSHA = deserialized_val
            case(56)
                energy%TG = deserialized_val
            case(57)
                energy%CH = deserialized_val
            case(58)
                energy%FCTR = deserialized_val
            case(59)
                energy%PAH = deserialized_val
            case(60)
                energy%TAUX = deserialized_val
            case(61)
                energy%FSH = deserialized_val
            case(62)
                energy%EMISSI = deserialized_val
            case(63)
                energy%TRAD = deserialized_val
            case(64)
                energy%APAR = deserialized_val
            case(65)
                energy%PSN = deserialized_val
            case(66)
                energy%STC = deserialized_val
            case(67)
                energy%LH = deserialized_val
			case(68)
                energy%TGS = deserialized_val
            case(69)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%HCPCT(sub_index) = deserialized_val
                end do
            case(70)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%DF(sub_index) = deserialized_val
                end do
            case(71)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%FACT(sub_index) = deserialized_val
                end do
            case(72)
                energy%PAHV = deserialized_val
            case(73)
                energy%PAHV = deserialized_val
            case(74)
                energy%PAHB = deserialized_val
            case(75)
                energy%FSHA = deserialized_val
            case(76)
                energy%LAISUN = deserialized_val
            case(77)
                energy%LAISHA = deserialized_val
            case(78)
                energy%BGAP = deserialized_val
            case(79)
                energy%WGAP = deserialized_val
            case(80)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%ALBD(sub_index) = deserialized_val
                end do
            case(81)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%ALBI(sub_index) = deserialized_val
                end do
            case(82)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%ALBGRD(sub_index) = deserialized_val
                end do
            case(83)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%ALBGRI(sub_index) = deserialized_val
                end do
            case(84)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%ALBSND(sub_index) = deserialized_val
                end do
            case(85)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%ALBSNI(sub_index) = deserialized_val
                end do
            case(86)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%FABD(sub_index) = deserialized_val
                end do
            case(87)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%FABI(sub_index) = deserialized_val
                end do
            case(88)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%FTDD(sub_index) = deserialized_val
                end do
            case(89)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%FTID(sub_index) = deserialized_val
                end do
            case(90)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%FTII(sub_index) = deserialized_val
                end do
            case(91)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%RHO(sub_index) = deserialized_val
                end do
            case(92)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%TAU(sub_index) = deserialized_val
                end do
            case(93)
                energy%FSUN = deserialized_val
            case(94)
                energy%TAUSS = deserialized_val
            case(95)
                energy%FAGE = deserialized_val
            case(96)
                energy%ALB = deserialized_val
            case(97)
                energy%ALBOLD = deserialized_val
            case(98)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%FTDI(sub_index) = deserialized_val
                end do
            case(99)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%FREVD(sub_index) = deserialized_val
                end do
            case(100)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%FREGD(sub_index) = deserialized_val
                end do
            case(101)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%FREVI(sub_index) = deserialized_val
                end do
            case(102)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%FREGI(sub_index) = deserialized_val
                end do
            case(103)
                energy%SAG = deserialized_val
            case(104)
                energy%SAV = deserialized_val
            case(105)
                energy%FSA = deserialized_val
            case(106)
                energy%PARSUN = deserialized_val
            case(107)
                energy%PARSHA = deserialized_val
            case(108)
                energy%FSR = deserialized_val
            case(109)
                energy%FSRV = deserialized_val
            case(110)
                energy%FSRG = deserialized_val
            case(111)
                energy%QSFC = deserialized_val
            case(112)
                energy%TV = deserialized_val
            case(113)
                energy%CAH2 = deserialized_val
            case(114)
                energy%IRB = deserialized_val
            case(115)
                energy%SHB = deserialized_val
            case(116)
                energy%EVB = deserialized_val
            case(117)
                energy%GHB = deserialized_val
            case(118)
                energy%TAUXB = deserialized_val
            case(119)
                energy%TAUYB = deserialized_val
            case(120)
                energy%EHB2 = deserialized_val
            case(121)
				energy%T2MB = deserialized_val
            case(122)
                energy%Q2B = deserialized_val
            case(123)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%IMELT(sub_index) = deserialized_val
                end do
            case(124)
                energy%QMELT = deserialized_val
            case(125)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    energy%SNOWT_AVG(sub_index) = deserialized_val
                end do
        end select
    end do   
END SUBROUTINE energy_deserialization
            

SUBROUTINE water_serialization (water, mp_arr)
    type(WaterType), intent(in) :: water
    class(mp_arr_type), allocatable, intent(out) :: mp_arr

    mp_arr = mp_arr_type(67)
    mp_arr%values(1)%obj = mp_float_type(water%FP) !FP
    mp_arr%values(2)%obj = mp_float_type(water%RAIN) !RAIN
    mp_arr%values(3)%obj = mp_float_type(water%SNOW) !SNOW
    mp_arr%values(4)%obj = mp_float_type(water%BDFALL) !BDFALL
    mp_arr%values(5)%obj = mp_float_type(water%QINTR) !QINTR
    mp_arr%values(6)%obj = mp_float_type(water%QDRIPR) !QDRIPR
    mp_arr%values(7)%obj = mp_float_type(water%QTHROR) !QTHROR
    mp_arr%values(8)%obj = mp_float_type(water%QINTS) !QINTS
    mp_arr%values(9)%obj = mp_float_type(water%QDRIPS) !QDRIPS
    mp_arr%values(10)%obj = mp_float_type(water%QTHROS) !QTHROS
    mp_arr%values(11)%obj = mp_float_type(water%QRAIN) !QRAIN
    mp_arr%values(12)%obj = mp_float_type(water%QSNOW) !QSNOW
    mp_arr%values(13)%obj = mp_float_type(water%SNOWHIN) !SNOWHIN
    mp_arr%values(14)%obj = mp_float_type(water%CANLIQ) !CANLIQ
    mp_arr%values(15)%obj = mp_float_type(water%CANICE) !CANICE
    mp_arr%values(16)%obj = mp_float_type(water%FWET) !FWET
    mp_arr%values(17)%obj = mp_float_type(water%CMC) !CMC
    mp_arr%values(18)%obj = mp_float_type(water%FSNO) !FSNO
    mp_arr%values(19)%obj = mp_float_type(water%BDSNO) !BDSNO
    mp_arr%values(20)%obj = mp_float_type(water%BTRAN) !BTRAN
    mp_arr%values(21)%obj = mp_array_type(water%BTRANI ) !BTRANI array (1:levels%NSOIL)
    mp_arr%values(22)%obj = mp_array_type(water%SNICEV ) !SNICEV array (-levels%NSNOW+1:0) # negative indexes
    mp_arr%values(23)%obj = mp_array_type(water%EPORE ) !EPORE array (-levels%NSNOW+1:0) # negative indexes
    mp_arr%values(24)%obj = mp_array_type(water%SNLIQV ) !SNLIQV array (-levels%NSNOW+1:0) # negative indexes
    mp_arr%values(25)%obj = mp_array_type(water%DF ) !DF  array (-levels%NSNOW+1:0) # negative indexes
    mp_arr%values(26)%obj = mp_array_type(water%HCPCT ) !HCPCT array (-levels%NSNOW+1:0) # negative indexes
    mp_arr%values(27)%obj = mp_array_type(water%SICE ) !SICE array (1:levels%NSOIL)
    mp_arr%values(28)%obj = mp_float_type(water%SNEQV) !SNEQV
    mp_arr%values(29)%obj = mp_float_type(water%SNOWH) !SNOWH
    mp_arr%values(30)%obj = mp_array_type(water%SH2O ) !SH2O array (1:levels%NSOIL)
    mp_arr%values(31)%obj = mp_array_type(water%SMC ) !SMC array (1:levels%NSOIL)
    mp_arr%values(32)%obj = mp_array_type(water%SNICE ) !SNICE array (-levels%NSNOW+1:0)
    mp_arr%values(33)%obj = mp_array_type(water%SNLIQ ) !SNLIQ array (-levels%NSNOW+1:0)
    mp_arr%values(34)%obj = mp_float_type(water%PONDING) !PONDING
    mp_arr%values(35)%obj = mp_float_type(water%SNEQVO) !SNEQVO
    mp_arr%values(36)%obj = mp_float_type(water%QVAP) !QVAP
    mp_arr%values(37)%obj = mp_float_type(water%QDEW) !QDEW
    mp_arr%values(38)%obj = mp_float_type(water%QSNSUB) !QSNSUB
    mp_arr%values(39)%obj = mp_float_type(water%QSEVA) !QSEVA
    mp_arr%values(40)%obj = mp_float_type(water%QSNFRO) !QSNFRO
    mp_arr%values(41)%obj = mp_float_type(water%QSDEW) !QSDEW
    mp_arr%values(42)%obj = mp_float_type(water%QINSUR) !QINSUR
    mp_arr%values(43)%obj = mp_float_type(water%ACSNOM) !ACSNOM
    mp_arr%values(44)%obj = mp_array_type(water%ETRANI ) !ETRANI array (1:levels%NSOIL)
    mp_arr%values(45)%obj = mp_float_type(water%RUNSRF) !RUNSRF
    mp_arr%values(46)%obj = mp_float_type(water%WSLAKE) !WSLAKE
    mp_arr%values(47)%obj = mp_float_type(water%EVAPOTRANS) !EVAPOTRANS
    mp_arr%values(48)%obj = mp_float_type(water%ECAN) !ECAN
    mp_arr%values(49)%obj = mp_float_type(water%ETRAN) !ETRAN
    mp_arr%values(50)%obj = mp_float_type(water%SNOFLOW) !SNOFLOW
    mp_arr%values(51)%obj = mp_float_type(water%PONDING1) !PONDING1
    mp_arr%values(52)%obj = mp_float_type(water%PONDING2) !PONDING2
    mp_arr%values(53)%obj = mp_integer_type(water%ISNOW) !ISNOW integer
    mp_arr%values(54)%obj = mp_array_type(water%DZSNSO ) !DZSNSO array (-levels%nsnow+1:levels%nsoil)
    mp_arr%values(55)%obj = mp_float_type(water%QSNBOT) !QSNBOT
    mp_arr%values(56)%obj = mp_float_type(water%RUNSUB) !RUNSUB
    mp_arr%values(57)%obj = mp_float_type(water%PDDUM) !PDDUM
    mp_arr%values(58)%obj = mp_float_type(water%runsrf_dt) !runsrf_dt
    mp_arr%values(59)%obj = mp_array_type(water%FCR ) !FCR array (1:levels%nsoil)
    mp_arr%values(60)%obj = mp_float_type(water%SICEMAX) !SICEMAX
    mp_arr%values(61)%obj = mp_float_type(water%FCRMAX) !FCRMAX
    mp_arr%values(62)%obj = mp_float_type(water%FACC) !FACC
    mp_arr%values(63)%obj = mp_float_type(water%QDRAIN) !QDRAIN
    mp_arr%values(64)%obj = mp_float_type(water%DEEPRECH) !DEEPRECH
    mp_arr%values(65)%obj = mp_float_type(water%ZWT) !ZWT
    mp_arr%values(66)%obj = mp_float_type(water%ASAT) !ASAT
    mp_arr%values(67)%obj = mp_float_type(water%SMCWTD) !SMCWTD

END SUBROUTINE water_serialization

SUBROUTINE water_deserialization (mp_arr, water)
    class(mp_arr_type), allocatable, intent(in) :: mp_arr
    type(WaterType), intent(inout) :: water
    real(kind=real64) :: deserialized_val
    class(mp_arr_type), allocatable :: mp_sub_arr
    logical :: status
    integer(kind=int64) :: index, sub_index 

    do index=1, size(mp_arr)
        if (index = 53) then
            call get_int(arr%values(index)%obj, deserialized_val, status)
        else if ((index >= 21 .AND. index <=27) .OR. (index >= 30 .AND. index <=33) .OR. (index = 44) .OR. (index = 54) .OR. (index = 59)) then
            if (is_arr(arr%values(index)%obj)) then
                mp_sub_arr = arr%values(index)%obj
            else
                !write to log file
            end if
        else    
            call get_real(arr%values(index)%obj, deserialized_val, status)
        end if
        select case(index)
            case(1)
                water%FP = deserialized_val
            case(2)
                water%RAIN = deserialized_val
            case(3)
                water%SNOW = deserialized_val
            case(4)
                water%BDFALL = deserialized_val
            case(5)
                water%QINTR = deserialized_val
            case(6)
                water%QDRIPR = deserialized_val
            case(7)
                water%QTHROR = deserialized_val
            case(8)
                water%QINTS = deserialized_val
            case(9)
                water%QDRIPS = deserialized_val
            case(10)
                water%QTHROS = deserialized_val
            case(11)
                water%QRAIN = deserialized_val
            case(12)
                water%QSNOW = deserialized_val
            case(13)
                water%SNOWHIN = deserialized_val
            case(14)
                water%CANLIQ = deserialized_val
            case(15)
                water%CANICE = deserialized_val
            case(16)
                water%FWET = deserialized_val
            case(17)
                water%CMC = deserialized_val
            case(18)
                water%FSNO = deserialized_val
            case(19)
                water%BDSNO = deserialized_val
            case(20)
                water%BTRAN = deserialized_val
            case(21)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    water%BTRANI(sub_index) = deserialized_val
                end do
            case(22)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    water%SNICEV(sub_index) = deserialized_val
                end do
            case(23)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    water%EPORE(sub_index) = deserialized_val
                end do
            case(24)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    water%SNLIQV(sub_index) = deserialized_val
                end do
            case(25)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    water%DF(sub_index) = deserialized_val
                end do
            case(26)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    water%HCPCT(sub_index) = deserialized_val
                end do
            case(27)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    water%SICE(sub_index) = deserialized_val
                end do
            case(28)
                water%SNEQV = deserialized_val
            case(29)
                water%SNOWH = deserialized_val
            case(30)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    water%SH2O(sub_index) = deserialized_val
                end do
            case(31)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    water%SMC(sub_index) = deserialized_val
                end do
            case(32)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    water%SNICE(sub_index) = deserialized_val
                end do
            case(33)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    water%SNLIQ(sub_index) = deserialized_val
                end do
            case(34)
                water%PONDING = deserialized_val
            case(35)
                water%SNEQVO = deserialized_val
            case(36)
                water%QVAP = deserialized_val
            case(37)
                water%QDEW = deserialized_val
            case(38)
                water%QSNSUB = deserialized_val
            case(39)
                water%QSEVA = deserialized_val
            case(40)
                water%QSNFRO = deserialized_val
            case(41)
                water%QSDEW = deserialized_val
            case(42)
                water%QINSUR = deserialized_val
            case(43)
                water%ACSNOM = deserialized_val
            case(44)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    water%ETRANI(sub_index) = deserialized_val
                end do
            case(45)
                water%RUNSRF = deserialized_val
            case(46)
                water%WSLAKE = deserialized_val
            case(47)
                water%EVAPOTRANS = deserialized_val
            case(48)
                water%ECAN = deserialized_val
            case(49)
                water%ETRAN = deserialized_val
            case(50)
                water%SNOFLOW = deserialized_val
            case(51)
                water%PONDING1 = deserialized_val
            case(52)
                water%PONDING2 = deserialized_val
            case(53)
                water%ISNOW = deserialized_val
            case(54)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    water%DZSNSO(sub_index) = deserialized_val
                end do
            case(55)
                water%QSNBOT = deserialized_val
            case(56)
                water%RUNSUB = deserialized_val
            case(57)
                water%PDDUM = deserialized_val
            case(58)
                water%runsrf_dt = deserialized_val
            case(59)
                do sub_index=1, size(mp_sub_arr)
                    call get_real(mp_sub_arr%values(sub_index)%obj, deserialized_val, status)
                    water%FCR(sub_index) = deserialized_val
                end do
            case(60)
                water%SICEMAX = deserialized_val
            case(61)
                water%FCRMAX = deserialized_val
            case(62)
                water%FACC = deserialized_val
            case(63)
                water%QDRAIN = deserialized_val
            case(64)
                water%DEEPRECH = deserialized_val
            case(65)
                water%ZWT = deserialized_val
            case(66)
                water%ASAT = deserialized_val
            case(67)
                water%SMCWTD = deserialized_val
        end select
    end do
END SUBROUTINE water_deserialization

SUBROUTINE parameters_serialization (parameters, mp_arr)
    type(ParametersType), intent(in) :: parameters
    class(mp_arr_type), allocatable, intent(out) :: mp_arr

    mp_arr = mp_arr_type(5)
    mp_arr%values(1)%obj = mp_float_type(parameters%SAI) !SAI
    mp_arr%values(2)%obj = mp_float_type(parameters%LAI) !LAI
    mp_arr%values(3)%obj = mp_float_type(parameters%ESAI) !ESAI
    mp_arr%values(4)%obj = mp_float_type(parameters%ELAI) !ELAI
    mp_arr%values(4)%obj = mp_float_type(parameters%FVEG) !FVEG

END SUBROUTINE parameters_serialization

SUBROUTINE parameters_deserialization (mp_arr, parameters)
    class(mp_arr_type), allocatable, intent(in) :: mp_arr
    type(ParametersType), intent(inout) :: parameters
    real(kind=real64) :: deserialized_val
    logical :: status
    integer(kind=int64) :: index

    do index=1, size(mp_arr)
        call get_real(arr%values(index)%obj, deserialized_val, status)
        select case(index)
            case(1)
                parameters%SAI = deserialized_val
            case(2)
                parameters%LAI = deserialized_val
            case(3)
                parameters%ESAI = deserialized_val
            case(4)
                parameters%ELAI = deserialized_val
            case(5)
                parameters%FVEG = deserialized_val
        end select
    end do
END SUBROUTINE parameters_deserialization

END Module
